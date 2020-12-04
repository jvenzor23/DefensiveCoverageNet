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
player_closing_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_closing_epas.csv")
player_ball_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_ball_skills_epas.csv")
player_tackling_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tackling_epas.csv")

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
  left_join(player_ball_skills) %>%
  left_join(player_tackling_skills) %>%
  mutate(eps_man_coverage = eps_tracking_w_penalties + eps_saved_closing_w_penalties +
           eps_saved_ball_skills_w_penalties + eps_tackling,
         eps_per_man_route = eps_man_coverage/routes) %>%
  dplyr::select("qualifying", "position", "displayName", "nflId_def", "routes", "targets",
         "eps_man_coverage","eps_tracking_w_penalties","eps_saved_closing_w_penalties",
         "eps_saved_ball_skills_w_penalties","eps_tackling") %>%
  arrange(desc(qualifying), desc(eps_man_coverage)) %>%
  filter(!is.na(eps_man_coverage))
