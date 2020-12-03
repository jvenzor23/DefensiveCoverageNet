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
# setwd("~/Desktop/NFL_PBP_DATA/")
# pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
#   select(play_id, game_id, air_yards, yards_after_catch, return_yards, fumble_lost) %>%
#   rename(gameId = game_id,
#          playId = play_id)

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  dplyr::select(play_id, game_id, air_yards, yards_after_catch) %>%
  rename(gameId = game_id,
         playId = play_id)

# Getting Penalties --------------------------------------------------------

plays_penalties = plays %>%
  filter(penaltyCodes != "") %>%
  dplyr::select(gameId, playId, playDescription, possessionTeam, penaltyCodes, penaltyJerseyNumbers,
                offensePlayResult, playResult, isDefensivePI) %>%
  inner_join(my_epa) %>%
  rowwise() %>%
  mutate(penalty_desc = trimws(str_split(str_split(str_to_lower(playDescription), "penalty")[[1]][2], ",")[[1]][2])) %>%
  mutate(enforced_flag = grepl("enforced", str_to_lower(playDescription)),
         declined_flag = grepl("declined", str_to_lower(playDescription)),
         offsetting_flag = grepl("offsetting", str_to_lower(playDescription))) %>%
  left_join(targeted_receiver)

penalty_freq = plays_penalties %>%
  group_by(penaltyCodes, penalty_desc) %>%
  summarize(count = n(),
            perc_enforced = mean(enforced_flag),
            perc_declined = mean(declined_flag),
            perc_offsetting = mean(offsetting_flag),
            avg_epa = mean(my_epa, na.rm = TRUE)
  ) %>%
  arrange(desc(count))


# Getting targeted WR depth at time of Incompletion -----------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

dpi_classification = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))

  pt_data_penalties = pt_data %>%
    inner_join(plays_penalties %>%
                 filter(isDefensivePI,
                        !offsetting_flag)) %>%
    inner_join(targeted_receiver %>%
                 rename(nflId = targetNflId)) %>%
    filter(grepl('pass_outcome_', event)) %>%
    mutate(catch_interference_play_result = round(pmin(99, x) - YardsFromOwnGoal)) %>%
    dplyr::select(gameId, playId, playResult, catch_interference_play_result) %>%
    mutate(closing_dpi = if_else(playResult - catch_interference_play_result < -3, 1, 0),
           ball_skills_dpi = if_else(playResult - catch_interference_play_result >= -3, 1, 0))

  dpi_classification = rbind(dpi_classification,
                             pt_data_penalties)

}

write.csv(dpi_classification,
          "~/Desktop/CoverageNet/src/04_evaluate_players/pass_interference_classification/outputs/dpi_classification.csv",
          row.names = FALSE)
