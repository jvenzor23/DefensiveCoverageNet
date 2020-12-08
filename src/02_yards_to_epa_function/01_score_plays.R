# This scores all plays in the Bg Data Bowl Data-set with their epas based
# on our expected points model. We find that the correlation between our
# epas and the ones supplied by the Big Data Bowl are very high (>.98).

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
library(nnet)


# Reading in the Data -----------------------------------------------------

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
  dplyr::select(game_id, play_id, posteam, play_type, game_half, qtr, half_seconds_remaining, ydstogo,
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


# Deriving Necessary Variables --------------------------------
pbp_data_2018_model_data = pbp_data_2018 %>%
  
  # Reference level should be No_Score:
  mutate(
         # log transform of yards to go and indicator for two minute warning:
         log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down)
         
  ) %>%
  filter(PlayType %in% c("field_goal", "no_play", "pass", "punt", "run", "sack",
                  "spike", "qb_kneel") 
         & is.na(TwoPointConv) & is.na(ExPointResult) & 
           !is.na(down) & !is.na(TimeSecs_Remaining))

base_preds = as.data.frame(predict(ep_model, pbp_data_2018_model_data, type = "probs")) %>%
  mutate(ep = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
         + 2*(Opp_Safety - Safety))

pbp_data_2018_model_data$my_ep = base_preds$ep

pbp_data_2018_model_data = pbp_data_2018_model_data %>%
  filter(!is.na(my_ep)&!is.na(ep))

# checking the correlations
cor(pbp_data_2018_model_data$ep, pbp_data_2018_model_data$my_ep)


# Computing Base EPA for All Plays ----------------------------------------
# change of possession based on posteam
# Cases to consider:
# 1) field_goal_result == "made" vs. "missed"
# 2) touchdown == 1
# 3) return_touchdown == 1
# 4) safety == 1
# 5) change of possession
# condition that if it is last play of half, its -ep!

pbp_data_2018_model_data2 = pbp_data_2018_model_data %>%
  # arrange(gameId, playId) %>%
  group_by(gameId, game_half) %>%
  mutate(my_epa = case_when(
                         field_goal_result == "made" ~ 3 - my_ep,
                         field_goal_result == "missed" ~ -lead(my_ep) - my_ep,
                         return_touchdown == 1 ~ -7 - my_ep,
                         (fumble_lost == 1) & (touchdown == 1) ~ -7 - my_ep,
                         touchdown == 1 ~ 7 - my_ep,
                         safety == 1 ~ -2 - my_ep,
                         row_number() == max(row_number()) ~ -my_ep,
                         lead(posteam) != posteam ~ -lead(my_ep) - my_ep,
                         TRUE ~ lead(my_ep) - my_ep)) %>%
  mutate(diff = abs(my_epa - epa))

# cor with Yurko
cor((pbp_data_2018_model_data2 %>%
      filter(!is.na(epa)&!is.na(my_epa)))$epa, 
    (pbp_data_2018_model_data2 %>%
      filter(!is.na(epa)&!is.na(my_epa)))$my_epa)

# cor with BDB epa
plays_with_my_epa = plays %>%
  dplyr::select(gameId, playId, playDescription, YardsFromOwnGoal,passResult, offensePlayResult,playResult, penaltyCodes, epa) %>%
  left_join(pbp_data_2018_model_data2 %>%
               ungroup() %>%
               dplyr::select(gameId, playId, air_yards, yards_after_catch, my_ep, my_epa))

plays_with_my_epa_check = plays_with_my_epa %>%
  filter(!is.na(my_epa))

cor(plays_with_my_epa_check$epa, plays_with_my_epa_check$my_epa)

# all plays are either spikes or penalties!
plays_missed = plays %>%
  anti_join(plays_with_my_epa_check)


# saving plays with my epa ------------------------------------------------
plays_epa_write = plays_with_my_epa %>%
  dplyr::select(gameId, playId, my_ep, my_epa)

write.csv(plays_epa_write,
          "~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv",
          row.names = FALSE)

check = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

