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

player_tracking_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tracking_eps.csv")
player_closing_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_closing_eps.csv")
player_ball_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_ball_skills_eps.csv")
player_tackling_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tackling_eps.csv")

pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv")
pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv")


# Getting On Percentage of Throws that Are on Target ---------------------------

perc_throws_on_target = dim(pass_arrived_epa)[1]/dim(pass_attempt_epa)[1]

# Getting On Target Throw Completion Percentage ---------------------------

on_target_comp_perc = (plays %>%
  inner_join(pass_arrived_epa %>%
               distinct(gameId, playId)) %>%
  summarize(comp_perc = mean(passResult == 'C')))$comp_perc




# Joining the Tables, and Summarizing -------------------------------------

skills_table = player_tracking_skills %>%
  left_join(player_closing_skills %>%
              dplyr::select(-targets)) %>%
  left_join(player_ball_skills %>%
              dplyr::select(-position)) %>%
  left_join(player_tackling_skills %>%
              dplyr::select(-position))

skills_table[is.na(skills_table)] = 0
 
skills_table = skills_table %>% 
  mutate(eps_man_coverage = eps_tracking_w_penalties + eps_saved_closing_w_penalties +
           eps_saved_ball_skills_w_penalties + eps_tackling + eps_int_returns + eps_ball_hawk,
         eps_man_coverage_no_penalties = eps_tracking + eps_saved_closing + 
           eps_saved_ball_skills + eps_tackling,
         eps_man_coverage_no_tackling = eps_man_coverage - eps_tackling,
         eps_per_man_route = eps_man_coverage/routes,
         penalties_count = tracking_penalities_count + closing_penalties + ball_skills_penalties,
         penalties_eps = tracking_penalties_eps + closing_penalties_eps + ball_skills_penalties_eps) %>%
  dplyr::select(-eps_tracking) %>%
  rename(tracking_penalities = tracking_penalities_count,
         eps_tracking = eps_tracking_w_penalties,
         eps_closing = eps_saved_closing_w_penalties,
         eps_ball_skills = eps_saved_ball_skills_w_penalties) %>%
  dplyr::select("position", "displayName", "nflId_def", "eps_man_coverage",
                "routes", "targets","completions", "PB", "ball_hawk_pbus", "interceptions","ball_hawk_ints", "Tackles", "FF", 
                "penalties_count", "tracking_penalities","closing_penalties","ball_skills_penalties",
                "penalties_eps", "tracking_penalties_eps", "closing_penalties_eps", "ball_skills_penalties",
                "eps_tracking","eps_closing","eps_ball_skills","eps_tackling","eps_int_returns", "eps_ball_hawk", 
                "eps_man_coverage_no_tackling") %>%
  rename(INT = interceptions,
         T = Tackles,
         completions_allowed = completions,
         accurate_targets = targets) %>%
  arrange(desc(eps_man_coverage)) %>%
  filter(!is.na(eps_man_coverage))

write.csv(skills_table,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/overall_player_skills_summary.csv",
          row.names = FALSE)


# By Route ----------------------------------------------------------------

route_player_tracking_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tracking_eps_by_route.csv")
route_player_closing_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_closing_eps_by_route.csv")
route_player_ball_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_ball_skills_eps_by_route.csv")
route_player_tackling_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tackling_eps_by_route.csv")

route_skills_table = route_player_tracking_skills %>%
  left_join(route_player_closing_skills %>%
              dplyr::select(-targets)) %>%
  left_join(route_player_ball_skills %>%
              dplyr::select(-position)) %>%
  left_join(route_player_tackling_skills %>%
              dplyr::select(-position))

route_skills_table[is.na(route_skills_table)] = 0

route_skills_table = route_skills_table %>% 
  mutate(eps_man_coverage = eps_tracking + eps_saved_closing +
           eps_saved_ball_skills + eps_tackling,
         eps_man_coverage_no_tackling = eps_man_coverage - eps_tackling,
         eps_per_man_route = eps_man_coverage/routes) %>%
  dplyr::select("position", "displayName", "route", "nflId_def", "eps_man_coverage",
                "routes", "targets",
                "completions", "PB", "interceptions", "Tackles", "FF",
                "eps_tracking","eps_saved_closing",
                "eps_saved_ball_skills","eps_tackling",
                "eps_man_coverage_no_tackling") %>%
  rename(INT = interceptions,
         T = Tackles,
         completions_allowed = completions,
         accurate_targets = targets) %>%
  arrange(route, desc(eps_man_coverage)) %>%
  filter(!is.na(eps_man_coverage))

write.csv(route_skills_table,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/overall_player_skills_summary_by_route.csv",
          row.names = FALSE)
