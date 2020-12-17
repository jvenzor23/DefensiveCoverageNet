# This code animates a given play in the player tracking data, and displays
# coverages.
#
# CONVENTION:
# 1) man-coverage: line connecting receiver and defender
# 2) zone-coverage: faded green circle around zone defender
# 3) **LBs are excluded, since the prompt asks for an analysis of
#    defensive backs. Defenders without (1) or (2) are either LBs or DLs

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/NFL_BIG_DATA_BOWL_2021/inputs/")

# Calling Necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)
library(rootSolve)
library(modeest)
library(gganimate)


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")


# Identify Rushers --------------------------------------------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

rushers_total = data.frame()
count = 0

for(file in files){
  
  
  pbp_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                             file))

  ball_snap_position = pbp_data %>%
    filter(is.na(nflId),
           event == "ball_snap") %>%
    dplyr::select(gameId, playId, x, y) %>%
    rename(x_snap = x,
           y_snap = y)
  
  defenders = pbp_data %>%
    filter(!is.na(nflId),
           !IsOnOffense) %>%
    dplyr::select(gameId, playId, nflId, event, frameId, x, y)
  
  football = pbp_data %>%
    filter(is.na(nflId)) %>%
    dplyr::select(gameId, playId, frameId, x, y) %>%
    rename(x_football = x,
           y_football = y)
  
  # computing percentage of frames after snap until throw in pocket
  # and the average distance from the ball
  
  rush_metrics = defenders %>%
    inner_join(ball_snap_position) %>%
    inner_join(football) %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId, nflId) %>%
    filter(cumsum(event == "ball_snap") >= 1) %>%
    group_by(gameId, playId, nflId) %>%
    filter(cumsum((event != "None")&(event != "ball_snap")) < 1) %>%
    ungroup() %>%
    mutate(in_pocket = if_else((x <= x_snap + 1) &
                               (abs(y - y_snap) <= 10),
                               1, 0),
           distance_from_ball = sqrt((x - x_football)^2 + (y - y_football)^2)) %>%
    group_by(gameId, playId, nflId) %>%
    summarize(perc_in_pocket = mean(in_pocket),
              avg_dist_from_ball = mean(distance_from_ball),
              distance_from_ball_at_end = distance_from_ball[length(distance_from_ball)])
  
  
  rushers = rush_metrics %>%
    filter(((perc_in_pocket > .125)|
           (avg_dist_from_ball < 7.5)|
           (distance_from_ball_at_end < 5)),
           avg_dist_from_ball < 15,
           perc_in_pocket > .05)
  
  if(count == 0){
    rushers_total = rbind.data.frame(rushers_total,
                          rushers) %>%
      arrange(gameId, playId, nflId)
  }else{
    rushers_total = rbind(rushers_total,
                          rushers) %>%
      arrange(gameId, playId, nflId)
  }
  
  count = count + 1
  
  write.csv(rushers_total,
            "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/rushers.csv",
            row.names = FALSE)
  
}

rushers_total = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/rushers.csv")

corners_pass_data = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_pass_attempts_man_zone_gmm_features.csv")
corners_sack_data = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_sacks_man_zone_gmm_features.csv")

corners_pass_data2 = corners_pass_data %>%
  anti_join(rushers_total)
corners_sack_data2 = corners_sack_data %>%
  anti_join(rushers_total)

write.csv(corners_pass_data2,
          "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_pass_attempts_man_zone_gmm_features_no_rushers.csv",
          row.names = FALSE)
write.csv(corners_sack_data2,
          "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_sacks_man_zone_gmm_features_no_rushers.csv",
          row.names = FALSE)

safeties_pass_data = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_pass_attempts_man_zone_gmm_features.csv")
safeties_sack_data = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_sacks_man_zone_gmm_features.csv")

safeties_pass_data2 = safeties_pass_data %>%
  anti_join(rushers_total)
safeties_sack_data2 = safeties_sack_data %>%
  anti_join(rushers_total)

write.csv(safeties_pass_data2,
          "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_pass_attempts_man_zone_gmm_features_no_rushers.csv",
          row.names = FALSE)
write.csv(safeties_sack_data2,
          "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_sacks_man_zone_gmm_features_no_rushers.csv",
          row.names = FALSE)

linebackers_pass_data = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/linebackers_pass_attempts_man_zone_gmm_features.csv")
linebackers_sack_data = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/linebackers_sacks_man_zone_gmm_features.csv")

linebackers_pass_data2 = linebackers_pass_data %>%
  anti_join(rushers_total)
linebackers_sack_data2 = linebackers_sack_data %>%
  anti_join(rushers_total)

write.csv(linebackers_pass_data2,
          "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/linebackers_pass_attempts_man_zone_gmm_features_no_rushers.csv",
          row.names = FALSE)
write.csv(linebackers_sack_data2,
          "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/linebackers_sacks_man_zone_gmm_features_no_rushers.csv",
          row.names = FALSE)


