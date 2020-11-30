# This code pulls plays for predicting the result of a pass at the time of attempt:
# RULES:
# 1) there must be a targeted receiver on the play
# -- this excludes sacks, spikes, throw-aways, etc.
# 2) there must not be any OPI, DPI, DH, or ICT
# -- we are using OffensePlayResult, so we can still use plays with any penalties
# that do not impact the catch (ie, roughing the passer). The only thing we want to exclude are cases where
# the play is broken up by a DPI, DH, or ICT,  or enabled by an OPI
# 3) there must be a pass_forward flag:
# -- this eliminates shovel passes, which we are fine with removing
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

plays %>% group_by(penaltyCodes) %>% summarize(count = n()) %>% arrange(desc(count))

check_event = pt_data %>%
  inner_join(plays) %>%
  filter(event != "None") %>%
  distinct(gameId, playId, passResult, event) %>%
  group_by(passResult, event) %>%
  summarize(count = n()) %>%
  arrange(passResult, desc(count))

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
    filter(grepl("DPI", penaltyCodes)|grepl("OPI", penaltyCodes)|
           grepl("DH", penaltyCodes)|grepl("ICT", penaltyCodes))
  
  # focusing on only completed passes prior to the end-zone (and no fumbles)
  fumble_plays = plays2 %>%
    filter(fumble_lost == 1) %>%
    select(gameId, playId) %>%
    distinct()
  
  clean_plays = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    anti_join(pass_interference_plays) %>%
    anti_join(fumble_plays) %>%
    filter(event == "pass_forward")
  
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

check = pass_attempt_total %>%
  distinct(week, gameId, playId) %>%
  group_by(week) %>%
  summarize(count = n())

# saving the data
write.csv(pass_attempt_total,
          "~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempts.csv",
          row.names = FALSE)

# Preparing Plays for Scoring -----------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

pass_attempt_score = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  # getting pass interference plays
  # pass_interference_plays = plays %>%
  #   filter(grepl("DPI", penaltyCodes)|grepl("OPI", penaltyCodes)|
  #            grepl("DH", penaltyCodes)|grepl("ICT", penaltyCodes))
  
  clean_plays = pt_data %>%
    inner_join(targeted_receiver %>%
                 filter(!is.na(targetNflId)) %>%
                 distinct(gameId, playId)) %>%
    # anti_join(pass_interference_plays) %>%
    filter(event == "pass_forward")
  
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
          "~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempts_with_fumbles.csv",
          row.names = FALSE)

