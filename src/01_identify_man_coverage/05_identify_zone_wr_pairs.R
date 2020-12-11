# This script identifies who a defender is covering, for defenders
# who were identified to be in man-coverage. It does so by finding
# the offensive player with the highest correlation of movement and
# the closest distance throughout the play.

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")

# man_zone_classification = rbind(
#   read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_pass_attempts_man_zone_classes.csv") %>%
#     dplyr::select(gameId, playId, nflId, zone_probability),
#   read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_sacks_man_zone_classes.csv") %>%
#     dplyr::select(gameId, playId, nflId, zone_probability),
#   read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_pass_attempts_man_zone_classes.csv") %>%
#     dplyr::select(gameId, playId, nflId, zone_probability),
#   read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_sacks_man_zone_classes.csv") %>%
#     dplyr::select(gameId, playId, nflId, zone_probability)
# ) %>%
#   arrange(gameId, playId, nflId)

man_zone_classification = rbind(
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_pass_attempts_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_sacks_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/linebackers_pass_attempts_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/linebackers_sacks_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability)
) %>%
  arrange(gameId, playId, nflId)


# Filtering to Players In Man Coverage ------------------------------------

# we require at least a 50% zone probability to consider a player in zone coverage
zone_coverage = man_zone_classification %>%
  filter(zone_probability >= .5)

# Identifying Closest Player ----------------------------------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

closest_zone_player_pairs_total = data.frame()

for(file in files){
  
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                             file))

  pt_data2 = pt_data %>%
    filter(!is.na(nflId),
           position != 'QB') %>%
    inner_join(pt_data %>%
                 filter(!is.na(nflId),
                        position != 'QB') %>%
                 dplyr::select(gameId, playId, frameId, team, nflId, x, y) %>%
                 rename(x_opp = x,
                        y_opp = y,
                        team_opp = team,
                        nflId_opp = nflId)) %>%
    filter(team != team_opp) %>%
    mutate(dist = sqrt((x - x_opp)^2 + (y- y_opp)^2)) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    filter(dist == min(dist)) %>%
    distinct(gameId, playId, frameId, nflId, .keep_all = TRUE)
  
  pt_data3 = pt_data2 %>%
    inner_join(pt_data2 %>%
                 dplyr::select(gameId, playId, frameId, nflId, nflId_opp) %>%
                 rename(nflId_opp_closest_player = nflId_opp),
               by = c("gameId", "playId", "frameId", "nflId_opp" = "nflId")) %>%
    filter(nflId_opp_closest_player == nflId,
           !IsOnOffense)
  
  closest_zone_player_pairs = pt_data3 %>%
    dplyr::select(gameId, playId, frameId, nflId, nflId_opp) %>%
    inner_join(zone_coverage %>%
                 dplyr::select(-zone_probability)) %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId) %>%
    mutate(comboId = cumsum(replace_na(lag(nflId) != nflId, FALSE)|
                            replace_na(lag(nflId_opp) != nflId_opp, FALSE)|
                            replace_na(lag(frameId) != (frameId - 1), FALSE))) %>%
    group_by(gameId, playId, comboId) %>%
    summarize(nflId = nflId[1],
              nflId_opp = nflId_opp[1],
              frameId_start = min(frameId),
              frameId_end = max(frameId)) %>%
    arrange(gameId, playId, nflId, frameId_start) %>%
    dplyr::select(-comboId)
  
  closest_zone_player_pairs_total = rbind.data.frame(closest_zone_player_pairs_total,
                                          closest_zone_player_pairs)
  
  write.csv(closest_zone_player_pairs_total,
            "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv",
            row.names = FALSE)
  
}
    
