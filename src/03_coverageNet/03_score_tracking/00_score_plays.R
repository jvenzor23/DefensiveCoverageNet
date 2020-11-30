# This code pulls plays for predicting the result of a pass before the time of a pass_forwards
# RULES:
# 1) C, I, and INT must have pass_forward flag
# 2) S will have whatever comes first for first_contact, qb_sack,
#    qb_strip_sack, and tackle

# This code implements the procedue from 02_score_attempt, but
# for each receiver on the field, for all times .5 seconds after the snap
# to the time of a throw (or sack if the play is a sack)

# A .5 second buffer was used, because prior to this point the 
# predicted epas will have more to do with scheme placement of
# player than player skil. 

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
library(nnet)

# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  select(play_id, game_id, air_yards, yards_after_catch, return_yards, fumble_lost) %>%
  rename(gameId = game_id,
         playId = play_id)

# 2018 BDB Plays
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", 
                 stringsAsFactors = FALSE) %>%
  mutate(YardsFromOwnGoal = if_else(yardlineSide == possessionTeam, 
                                    as.numeric(yardlineNumber), 
                                    50 + (50-yardlineNumber)),
         YardsFromOwnGoal = if_else(yardlineNumber == 50, 
                                    50, 
                                    as.numeric(YardsFromOwnGoal)))

# 2018 NFL Full Play-By-Play Data
setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read.csv("reg_pbp_2018.csv", stringsAsFactors = FALSE) %>%
  select(game_id, play_id, posteam, play_type, game_half, qtr, half_seconds_remaining, ydstogo,
         down, yardline_100, yards_gained,
         touchdown, return_touchdown, safety, field_goal_result,
         goal_to_go, 
         air_yards, yards_after_catch, return_yards,
         extra_point_result, two_point_conv_result, fumble_lost,
         interception, fourth_down_failed,
         ep, epa) %>%
  rename(gameId = game_id,
         playId = play_id,
         TimeSecs_Remaining = half_seconds_remaining,
         GoalToGo = goal_to_go,
         yrdline100 = yardline_100,
         PlayType = play_type,
         TwoPointConv = two_point_conv_result,
         ExPointResult = extra_point_result)

# loading model
load(file="~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/ep_model.RData")
# my epa data
my_epa_data = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]
files = files[16:17]

epa_output_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa2.csv")
# epa_output_total = data.frame()
counter = 1

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  print(paste(file, "player tracking data read!"))
  
  # (1) Getting the Cutoffs -----------------------------------------------------
  
  source("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/funs/00_get_tracking_pt_data.R")
  pass_attempts_full = get_tracking_pt_data(pt_data,
                                       plays)
  
  nSplits = 4
  
  # splitting up the data so it is small enough to handle (into 4 pieces!)
  pass_attempts_plays = pass_attempts_full %>%
    distinct(gameId, playId, targetNflId, frameId) %>%
    ungroup() %>%
    mutate(group = ceiling(nSplits*row_number()/max(row_number())))
  
  for(split in 1:nSplits){
    
    print(paste(file, split, "started!"))
  
    pass_attempts = pass_attempts_full %>%
      inner_join(pass_attempts_plays %>%
                   filter(group == split) %>%
                   select(-group)) 
    
    # (2) Creating the Tensor Data ------------------------------------------------
    source("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/funs/01_get_tensor.R")
    tensor_data = get_tensor(pass_attempts)
    
    write.csv(tensor_data,
              "~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_tensor_score.csv",
              row.names = FALSE)
    
    rm(tensor_data)
    
    
    # (3) Run Python Script to Score Tensors --------------------------------------
    
    setwd("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/funs")
    use_python("/anaconda3/bin/python", required = T)
    # CHANGE NAME HERE TO FILE!!!!!!!!!
    py_run_file('02_apply_models.py')
    
    
    # (4) Getting EPA For All Plays -----------------------------------------------
    
    setwd("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/")
    system('rm ~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_tensor_score.csv')
    
    # 2018 Pass Arrived Classifications
    pass_attempt_class_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_pass_attempt_classification_probs.csv")
    # 2018 Catch Yards distributions
    pass_attempt_caught_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_pass_caught_yards_probs.csv")
    # 2018 Int distribtuions
    pass_attempt_intercepted_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/intermediate_interceptions_yards_probs.csv")
    
    source("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/funs/03_get_epas.R")
    epa_output = get_epas(pass_attempt_class_preds,
                          pass_attempt_caught_preds,
                          pass_attempt_intercepted_preds,
                          pbp_data_2018)
    
    epa_output = epa_output %>%
      mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1])) %>%
      select(week, gameId, playId, targetNflId, frameId, everything())
    
    # deleting intermediate files!
    setwd("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/")
    system('rm ~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/intermediates/*.csv')
    
    # Writing to the output ---------------------------------------------------
    
    counter = counter + 1
    
    if(counter == 1){
      epa_output_total = rbind.data.frame(epa_output_total, epa_output
      ) %>%
        arrange(week, gameId, playId, targetNflId, frameId)
    }else{
      epa_output_total = rbind(epa_output_total, epa_output
      ) %>%
        arrange(week, gameId, playId, targetNflId, frameId)
    }
    
    write.csv(epa_output_total,
              "~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv",
              row.names = FALSE)
    
    rm(pass_attempt_class_preds,
       pass_attempt_caught_preds,
       pass_attempt_intercepted_preds,
       epa_output)
    
    print(paste(file, split, "completed!"))
    
  }
  
}

