# This code pulls plays for predicting the result of a pass as it arrives:
# RULES:
# 1) there must be a targeted receiver on the play
# -- this excludes sacks, spikes, throw-aways, etc.
# 2) there must not be any OPI or DPI
# -- we are using OffensePlayResult, so we can still use plays with any penalties
# prior (or after) the catch. The only thing we want to exclude are cases where
# the play is broken up by a DPI, or enabled by an OPI
# 3) there must be a pass arrived flag:
# -- for this, we care about how players disrupt ON TARGET passes only.
# If a throw is errant, it will look like the DB did good by default.
# Errant throws do not have a pass arrived flag, and so these plays should
# not be analyzed from the standpoint of PBU skill/closing ability, since
# the ball is not on target. All of the skill should be attributed to pre-pass
# positioning that contributed to the errant throw.
# By filtering to only plays with pass-arrived, we ensure that this is always
# the case.
# 4) plays with fumbles are removed from the training data-set, but are included
# -- in a total scoring dataset

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


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  select(play_id, game_id, air_yards, yards_after_catch, return_yards, fumble_lost) %>%
  rename(gameId = game_id,
         playId = play_id)

pt_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week1.csv")


# Preparing the Plays Df --------------------------------------------------

plays2 = plays %>%
  left_join(pbp_data_2018) %>%
  mutate(offensePlayResult = if_else(passResult == "IN",
                                     air_yards - return_yards,
                                     as.numeric(offensePlayResult)))

# Preparing Plays for Training -----------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

pass_attempt_total = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))

  # getting pass interference plays
  pass_interference_plays = plays %>%
    filter(grepl("DPI", penaltyCodes)|grepl("OPI", penaltyCodes))
  
  # focusing on only completed passes prior to the end-zone (and no fumbles)
  fumble_plays = plays2 %>%
    filter(fumble_lost == 1) %>%
    select(gameId, playId) %>%
    distinct()
  
  clean_pt_data = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    anti_join(fumble_plays) %>%
    inner_join(
      pt_data %>%
        filter(event == "pass_arrived") %>%
        distinct(gameId, playId)
    )
  
  within_2_5_yards = clean_pt_data %>%
    inner_join(targeted_receiver) %>%
    filter((nflId == targetNflId)|is.na(nflId)) %>%
    group_by(gameId, playId, frameId, event) %>%
    summarize(receiver_dist = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)) %>%
    group_by(gameId, playId) %>%
    filter(cumsum(replace_na(lag(1*(event == "pass_arrived")), 0)) == 0) %>%
    filter((max(frameId) - frameId) <= 5) %>%
    filter(abs(receiver_dist - 2.5) == min(abs(receiver_dist - 2.5))) %>%
    select(gameId, playId, frameId)
    
  full_arrival_times = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    anti_join(fumble_plays) %>%
    filter(event == "pass_arrived") %>%
    distinct(gameId, playId, frameId) %>%
    left_join(within_2_5_yards %>%
                rename(frameIdnew = frameId)) %>%
    mutate(frameId = if_else(is.na(frameIdnew),
                             frameId,
                             frameIdnew)) %>%
    select(gameId, playId, frameId)
    
  
  clean_plays = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    anti_join(fumble_plays) %>%
    inner_join(full_arrival_times) %>%
    distinct(gameId, playId, frameId)
  
  pt_data2 = pt_data %>%
    inner_join(clean_plays) %>%
    inner_join(targeted_receiver) %>%
    inner_join(plays2 %>%
                 select(gameId, playId, passResult, offensePlayResult)) %>%
    filter(!is.na(offensePlayResult)) %>%
    mutate(IsReceiver = targetNflId == nflId) %>%
    select(-time) %>%
    group_by(gameId, playId, frameId) %>%
    # ensuring exactly one player listed as targeted receiver
    filter(sum(1*IsReceiver, na.rm = TRUE) == 1) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1])) %>%
    select(-playDirection, -route)
  
  if(file == "week1.csv"){
    pass_attempt_total = rbind.data.frame(pass_attempt_total, pt_data2
    ) %>%
      arrange(week, gameId, playId, nflId)
  }else{
    pass_attempt_total = rbind(pass_attempt_total, pt_data2
    ) %>%
      arrange(week, gameId, playId, nflId)
  }
  
  print(paste(file, "completed!"))
  
}


# saving the data
write.csv(pass_attempt_total,
          "~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts.csv",
          row.names = FALSE)

# Preparing Plays for Scoring -----------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

pass_attempt_score = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  # getting pass interference plays
  pass_interference_plays = plays %>%
    filter(grepl("DPI", penaltyCodes)|grepl("OPI", penaltyCodes))
  
  clean_pt_data = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    inner_join(
      pt_data %>%
        filter(event == "pass_arrived") %>%
        distinct(gameId, playId)
    )
  
  within_2_5_yards = clean_pt_data %>%
    inner_join(targeted_receiver) %>%
    filter((nflId == targetNflId)|is.na(nflId)) %>%
    group_by(gameId, playId, frameId, event) %>%
    summarize(receiver_dist = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)) %>%
    group_by(gameId, playId) %>%
    filter(cumsum(replace_na(lag(1*(event == "pass_arrived")), 0)) == 0) %>%
    filter((max(frameId) - frameId) <= 5) %>%
    filter(abs(receiver_dist - 2.5) == min(abs(receiver_dist - 2.5))) %>%
    select(gameId, playId, frameId)
  
  full_arrival_times = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    filter(event == "pass_arrived") %>%
    distinct(gameId, playId, frameId) %>%
    left_join(within_2_5_yards %>%
                rename(frameIdnew = frameId)) %>%
    mutate(frameId = if_else(is.na(frameIdnew),
                             frameId,
                             frameIdnew)) %>%
    select(gameId, playId, frameId)
  
  
  clean_plays = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    inner_join(full_arrival_times) %>%
    distinct(gameId, playId, frameId)
  
  pt_data2 = pt_data %>%
    inner_join(clean_plays) %>%
    inner_join(targeted_receiver) %>%
    inner_join(plays2 %>%
                 select(gameId, playId, passResult, offensePlayResult)) %>%
    filter(!is.na(offensePlayResult)) %>%
    mutate(IsReceiver = targetNflId == nflId) %>%
    select(-time) %>%
    group_by(gameId, playId, frameId) %>%
    # ensuring exactly one player listed as targeted receiver
    filter(sum(1*IsReceiver, na.rm = TRUE) == 1) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1])) %>%
    select(-playDirection, -route)
  
  if(file == "week1.csv"){
    pass_attempt_score = rbind.data.frame(pass_attempt_score, pt_data2
    ) %>%
      arrange(week, gameId, playId, nflId)
  }else{
    pass_attempt_score = rbind(pass_attempt_score, pt_data2
    ) %>%
      arrange(week, gameId, playId, nflId)
  }
  
  print(paste(file, "completed!"))
  
}

# saving the data
write.csv(pass_attempt_score,
          "~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv",
          row.names = FALSE)
