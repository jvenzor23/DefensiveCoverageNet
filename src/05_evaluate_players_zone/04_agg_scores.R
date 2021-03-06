# This code scores player's tackling ability
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
# 4) For interceptions, we need to ensure it was the player in man coverage
#    (not another player coming in to help in a zone) that actually made the
#    interception!

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/CoverageNet/inputs/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
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

player_closing_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_closing_eps.csv")
player_ball_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_ball_skills_eps.csv")
player_tackling_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_tackling_eps.csv")

pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv")
pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv")

man_zone_classification = rbind(
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_pass_attempts_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_sacks_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability)
) %>%
  arrange(gameId, playId, nflId) %>%
  distinct(gameId, playId, nflId, .keep_all = TRUE)

man_coverage = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")

wr_db_zone_matchups_tot = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")


# Filtering to Players In Man Coverage ------------------------------------

# we take anyone who wasn't identified in man and make them zone
zone_coverage = man_zone_classification %>%
  anti_join(man_coverage,
            by = c("gameId", "playId", "nflId" = "nflId_def"))

zone_coverage_counts = zone_coverage %>%
  ungroup() %>%
  distinct(gameId, playId, nflId) %>%
  group_by(nflId) %>%
  summarize(zone_plays = n()) %>%
  arrange(desc(zone_plays)) %>%
  rename(nflId_def = nflId) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position) %>%
               rename(nflId_def = nflId))

zone_coverage_cover_counts = wr_db_zone_matchups_tot %>% 
  ungroup() %>%
  distinct(gameId, playId, nflId) %>%
  group_by(nflId) %>%
  summarize(zone_covers = n()) %>%
  arrange(desc(zone_covers)) %>%
  rename(nflId_def = nflId)

# Joining the Tables, and Summarizing -------------------------------------

skills_table = zone_coverage_counts %>%
  dplyr::select(-position, -displayName) %>%
  full_join(zone_coverage_cover_counts) %>%
  full_join(player_closing_skills %>%
              dplyr::select(-targets, -qualifying, -displayName, -position)) %>%
  full_join(player_ball_skills %>%
              dplyr::select(-position, -qualifying, -displayName, -routes)) %>%
  full_join(player_tackling_skills %>%
              dplyr::select(-position, -qualifying, -displayName)) %>%
  inner_join(players %>%
               dplyr::select(position, displayName, nflId),
             by = c("nflId_def" = "nflId"))

skills_table[is.na(skills_table)] = 0
 
skills_table = skills_table %>% 
  mutate(eps_zone_coverage =  eps_saved_closing_w_penalties +
           eps_saved_ball_skills_w_penalties + eps_tackling + eps_int_returns + eps_ball_hawk,
         eps_zone_coverage_no_penalties =  eps_saved_closing + 
           eps_saved_ball_skills + eps_tackling,
         eps_zone_coverage_no_tackling = eps_zone_coverage - eps_tackling,
         penalties_count =  closing_penalties + ball_skills_penalties,
         penalties_eps =  closing_penalties_eps + ball_skills_penalties_eps) %>%
  rename(eps_closing = eps_saved_closing_w_penalties,
         eps_ball_skills = eps_saved_ball_skills_w_penalties) %>%
  dplyr::select("position", "displayName", "nflId_def", "eps_zone_coverage",
                "zone_plays", "zone_covers", "targets","completions", "PB", "ball_hawk_pbus", "interceptions","ball_hawk_ints", "Tackles", "FF", 
                "penalties_count","closing_penalties","ball_skills_penalties",
                "penalties_eps", "closing_penalties_eps", "ball_skills_penalties",
                "eps_closing","eps_ball_skills","eps_tackling","eps_int_returns", "eps_ball_hawk", 
                "eps_zone_coverage_no_tackling") %>%
  rename(INT = interceptions,
         T = Tackles,
         completions_allowed = completions,
         accurate_targets = targets) %>%
  arrange(desc(eps_zone_coverage)) %>%
  filter(!is.na(eps_zone_coverage))

write.csv(skills_table,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/overall_player_skills_summary.csv",
          row.names = FALSE)

check = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/overall_player_skills_summary.csv")

check2 = skills_table %>%
  full_join(check,
             by = c("position", "displayName", "nflId_def")) %>%
  mutate(eps_tot = eps_zone_coverage + eps_man_coverage) %>%
  dplyr::select("position", "displayName", "nflId_def",
                "eps_tot", "eps_zone_coverage","eps_man_coverage") %>%
  arrange(desc(eps_tot))

# By Route ----------------------------------------------------------------
routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

route_zone_coverage_cover_counts = wr_db_zone_matchups_tot %>% 
  inner_join(routes,
             by = c("gameId", "playId", "nflId_opp" = "nflId")) %>%
  ungroup() %>%
  distinct(gameId, playId, nflId, route) %>%
  group_by(nflId, route) %>%
  summarize(zone_covers = n()) %>%
  arrange(desc(zone_covers)) %>%
  rename(nflId_def = nflId)

route_player_closing_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_closing_eps_by_route.csv")
route_player_ball_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_ball_skills_eps_by_route.csv")
route_player_tackling_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_tackling_eps_by_route.csv")

route_skills_table = route_player_closing_skills %>%
              dplyr::select(-targets, -position, -displayName) %>%
  full_join(route_player_ball_skills %>%
              dplyr::select(-position, -displayName)) %>%
  full_join(route_player_tackling_skills %>%
              dplyr::select(-position, -displayName)) %>%
  inner_join(players %>%
               dplyr::select(position, displayName, nflId),
             by = c("nflId_def" = "nflId")) %>%
  left_join(route_zone_coverage_cover_counts)

route_skills_table[is.na(route_skills_table)] = 0

route_skills_table = route_skills_table %>% 
  mutate(eps_zone_coverage =  eps_saved_closing +
           eps_saved_ball_skills + eps_tackling,
         eps_zone_coverage_no_tackling = eps_zone_coverage - eps_tackling) %>%
  rename(eps_closing = eps_saved_closing,
         eps_ball_skills = eps_saved_ball_skills) %>%
  dplyr::select("position", "displayName", "route", "nflId_def", "eps_zone_coverage",
                "zone_covers","targets",
                "completions", "PB", "interceptions", "Tackles", "FF",
                "eps_closing",
                "eps_ball_skills","eps_tackling",
                "eps_zone_coverage_no_tackling") %>%
  rename(INT = interceptions,
         T = Tackles,
         completions_allowed = completions,
         accurate_targets = targets) %>%
  arrange(route, desc(eps_zone_coverage)) %>%
  filter(!is.na(eps_zone_coverage))

write.csv(route_skills_table,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/overall_player_skills_summary_by_route.csv",
          row.names = FALSE)

