# This code scores players tracking ability
# RULES:
# 1) all plays where a player commits a penalty during the tracking phase are excluded
#    from scoring based on our epa tracking values, and will be assinged the negative
#    epa value associated with the penalty
# -- this includes defensive holding, illegal contact, and illegal use of hands
# 2) any accepted penalty will be held against a player, while any declined
#    or offsetting penalty will not be held against a player (but that player
#    will not be evaluated based on their tracking with epa values for that
#    play!)
# 3) Unfortunately, plays with accepted penalties do not include routes.
#    As a result, the valid data on all non-penalized players are not
#    included in this analysis. To fix, we would update the 00_get_tracking_pt_data.R
#    function called in 00_score_plays.R to include these players (although
#    we still would not know their route without doing much more work to build
#    a classification algorithm based on assigned routes)

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/CoverageNet/inputs/")

# Calling Necessary Libraries
library(tidyverse)
# library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)
library(gganimate)
library(magick)
library(fitdistrplus)
library(skewt)
library(sn)
library(broom)
library(dplyr)


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")


wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
wr_db_man_matchups = wr_db_man_matchups %>%
  group_by(gameId, playId, nflId_off) %>%
  filter(n() == 1)
wr_db_zone_matchups_tot = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")

epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

zone_matchups = epa_tracking_total %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  group_by(gameId, playId, nflId, targetNflId, frameId_start) %>%
  mutate(nFrames = max(frameId) - min(frameId) + 1) %>%
  filter(nFrames >= 5) %>%
  dplyr::select(-nFrames) %>%
  distinct(gameId, playId, nflId, targetNflId, frameId_start)
  

pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_attempt) %>%
  rename(comp_prob_pass_attempt = C_prob)

pass_attempt_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_arrived) %>%
  rename(comp_prob_pass_arrived = C_prob)

pass_arrived_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

pass_result_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")

receiver_man_tracking_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_tracking_eps.csv")
receiver_zone_tracking_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_zone_tracking_eps.csv")
receiver_ball_skills_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_ball_skills_eps.csv")
receiver_closing_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_closing_eps.csv")
receiver_tackling_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_tackling_eps.csv")

# Compute Strength of Schedule --------------------------------------------

# man tracking difficulty
man_tracking_sos = wr_db_man_matchups %>%
                    left_join(receiver_man_tracking_eps,
                              by = c("nflId_off" = "targetNflId")) %>%
  dplyr::select(gameId, playId, nflId_def, eps_per_route)

man_tracking_sos[is.na(man_tracking_sos)] = (receiver_man_tracking_eps %>% filter(is.na(targetNflId)))$eps_per_route

man_tracking_sos2 = man_tracking_sos %>%
  group_by(nflId_def) %>%
  summarize(routes = n(),
            expected_eps = sum(eps_per_route),
            sos = mean(eps_per_route)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, routes, everything()) %>%
  arrange(desc(routes))

# zone tracking difficulty
zone_tracking_sos = zone_matchups %>%
  left_join(receiver_zone_tracking_eps,
            by = c("targetNflId" = "targetNflId")) %>%
  dplyr::select(gameId, playId, nflId, eps_per_route)

zone_tracking_sos[is.na(zone_tracking_sos)] = (receiver_zone_tracking_eps %>% filter(is.na(targetNflId)))$eps_per_route

zone_tracking_sos2 = zone_tracking_sos %>%
  group_by(nflId) %>%
  summarize(routes = n(),
            expected_eps = sum(eps_per_route),
            sos = mean(eps_per_route)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId" = "nflId")) %>%
  rename(nflId_def = nflId) %>%
  dplyr::select(position, displayName, nflId_def, routes, everything()) %>%
  arrange(desc(routes))

# closing difficulty ------------------------------------------------------
pass_attempt_zone_link = pass_attempt_epa %>%
  inner_join(pass_attempt_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

pass_arrived_zone_link = pass_arrived_epa %>%
  inner_join(pass_arrived_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

wr_db_zone_closing_matchups = pass_attempt_zone_link %>%
  inner_join(pass_arrived_zone_link)

wr_db_man_closing_matchups = pass_attempt_epa %>%
  inner_join(pass_arrived_epa) %>%
  inner_join(targeted_receiver) %>%
  filter(!is.na(targetNflId)) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  rename(nflId_off = targetNflId) %>%
  dplyr::select(names(wr_db_zone_closing_matchups))

tot_closing_matchps = rbind(wr_db_zone_closing_matchups,
                            wr_db_man_closing_matchups)

closing_sos = tot_closing_matchps %>%
  left_join(receiver_closing_eps,
            by = c("nflId_off" = "targetNflId")) %>%
    dplyr::select(gameId, playId, nflId_off, eps_saved_closing_per_target_w_penalties)

closing_sos[is.na(closing_sos)] = (receiver_closing_eps %>% filter(is.na(targetNflId)))$eps_saved_closing_per_target_w_penalties

closing_sos2 = closing_sos %>%
  group_by(nflId_def) %>%
  summarize(accurate_targets = n(),
            expected_closing_eps = sum(eps_per_route),
            closing_sos = mean(eps_per_route)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, accurate_targets, everything()) %>%
  arrange(desc(accurate_targets))
