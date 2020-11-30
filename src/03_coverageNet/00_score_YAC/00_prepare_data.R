# This code pulls plays for predicting the result of a pass after it was caught
# RULES:
# 1) there must be a targeted receiver on the play
# -- this excludes sacks, spikes, throw-aways, etc.
# 2) the pass must not be caught in the end-zone (since YAC is irrelevant then)
# 3) training data will not include fumbles, but plays with fumbles will be scored

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


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")


setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  select(play_id, game_id, air_yards, yards_after_catch) %>%
  rename(gameId = game_id,
         playId = play_id)

# Preparing Completion Plays -------------------------------------------------
  
setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

pass_reception_total = data.frame()

for(file in files){

  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                             file))
  
  # focusing on only completed passes prior to the end-zone (and no fumbles)
  fumble_caught_td_plays = pt_data %>%
    filter(grepl("fumble", event)|(event == "pass_outcome_touchdown")) %>%
    select(gameId, playId) %>%
    distinct()
  
  clean_plays = pt_data %>%
    anti_join(fumble_caught_td_plays) %>%
    filter(event == "pass_outcome_caught") %>%
    select(gameId, playId, frameId) %>%
    distinct()
  
  pt_data2 = pt_data %>%
    inner_join(clean_plays) %>%
    inner_join(pbp_data_2018) %>%
    inner_join(targeted_receiver) %>%
    inner_join(plays %>%
                 select(gameId, playId, playResult)) %>%
    filter(!is.na(air_yards)) %>%
    filter(!is.na(yards_after_catch)) %>%
    mutate(IsReceiver = targetNflId == nflId) %>%
    filter(!is.na(nflId)) %>%
    select(-time) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1]))
  
  pass_reception_total = rbind(pass_reception_total, pt_data2) %>%
    arrange(week, gameId, playId, nflId)
  
  print(paste(file, "completed!"))
  
}

check = pass_reception_total %>%
  distinct(week, gameId, playId) %>%
  group_by(week) %>%
  summarize(count = n())

write.csv(pass_reception_total,
          "~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/pass_attempts.csv",
          row.names = FALSE)

# pulling ALL plays with a catch, except in the endzone (so now includes fumbles)
setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

pass_reception_all_total = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  # focusing on only completed passes prior to the end-zone (and no fumbles)
  caught_td_plays = pt_data %>%
    filter((event == "pass_outcome_touchdown")) %>%
    select(gameId, playId) %>%
    distinct()
  
  clean_plays = pt_data %>%
    anti_join(caught_td_plays) %>%
    filter(event == "pass_outcome_caught") %>%
    select(gameId, playId, frameId) %>%
    distinct()
  
  pt_data2 = pt_data %>%
    inner_join(clean_plays) %>%
    inner_join(pbp_data_2018) %>%
    inner_join(targeted_receiver) %>%
    inner_join(plays %>%
                 select(gameId, playId, playResult)) %>%
    filter(!is.na(air_yards)) %>%
    filter(!is.na(yards_after_catch)) %>%
    mutate(IsReceiver = targetNflId == nflId) %>%
    filter(!is.na(nflId)) %>%
    select(-time) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1]))
  
  pass_reception_all_total = rbind(pass_reception_all_total, pt_data2) %>%
    arrange(week, gameId, playId, nflId)
  
  print(paste(file, "completed!"))
  
}

# saving the data
write.csv(pass_reception_all_total,
          "~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/pass_attempts_all.csv",
          row.names = FALSE)

# Getting Interceptions ---------------------------------------------------

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  select(play_id, game_id, air_yards, return_yards, interception_player_id, interception_player_name, penalty) %>%
  rename(gameId = game_id,
         playId = play_id)

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

int_total = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))

  # focusing on only completed passes prior to the end-zone (and no fumbles)
  int_plays_events = pt_data %>%
    inner_join(plays %>%
                 filter(passResult == "IN") %>%
                 distinct(gameId,playId)) %>%
    distinct(gameId, playId, event) %>%
    group_by(event) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  # focusing on only completed passes prior to the end-zone (and no fumbles)
  fumble_caught_td_plays = pt_data %>%
    filter(grepl("fumble", event)|(event == "pass_outcome_touchdown")) %>%
    select(gameId, playId) %>%
    distinct()
  
  clean_plays = pt_data %>%
    anti_join(fumble_caught_td_plays) %>%
    filter(event == "pass_outcome_interception") %>%
    select(gameId, playId, frameId) %>%
    distinct()
  
  pt_data2 = pt_data %>%
    inner_join(clean_plays) %>%
    inner_join(pbp_data_2018) %>%
    inner_join(plays %>%
                 select(gameId, playId, playResult)) %>%
    filter(penalty == 0,
           !is.na(nflId)) %>%
    rename(yards_after_catch = return_yards) %>%
    filter(!is.na(yards_after_catch)) %>%
    separate(displayName, into = c("first", "rest"), sep = "\\s",
             extra = "merge") %>%
    mutate(displayName = paste0(substring(first, 1, 1), ".",
                                rest)) %>%
    mutate(displayName = trimws(str_remove(displayName, "II|III|Jr|Jr."))) %>%
    mutate(interception_player_name = trimws(str_remove(interception_player_name, "II|III|Jr|Jr."))) %>%
    mutate(IsReceiver = displayName == interception_player_name) %>%
    filter(!is.na(IsReceiver)) %>%
    select(-time) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1]))
  
  plays_with_interceptors = pt_data2 %>%
    ungroup() %>%
    group_by(gameId, playId) %>%
    summarize(count = sum(IsReceiver)) %>%
    filter(count == 1) %>%
    select(-count)
  
  pt_data3 = pt_data2 %>%
    inner_join(plays_with_interceptors) %>%
    mutate(x = 120 - x,
           y = 160/3-y,
           dir = ifelse(dir < 90, dir + 180, dir - 180),
           IsOnOffense = !IsOnOffense)
  
  int_total = rbind(int_total, pt_data3) %>%
    arrange(week, gameId, playId, nflId)
  
  print(paste(file, "completed!"))
  
}

check = int_total %>%
  distinct(week, gameId, playId) %>%
  group_by(week) %>%
  summarize(count = n())

write.csv(int_total,
          "~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/interceptions.csv",
          row.names = FALSE)
