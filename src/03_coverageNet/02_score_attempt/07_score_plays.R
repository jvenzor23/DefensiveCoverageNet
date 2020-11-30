# This code translates the yardage and C/I/INT predictions into epa
# values

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
# 2018 Pass Arrived Classifications
pass_attempt_class_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_classification_probs.csv")
# 2018 Catch Yards distribtions
pass_attempt_caught_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_caught_yards_probs.csv")
# 2018 Int distribtions
pass_attempt_intercepted_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/interceptions_yards_probs.csv")
# loading model
load(file="~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/ep_model.RData")
# my epa data
my_epa_data = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

# Deriving Necessary Variables --------------------------------

# cor with BDB epa
plays_with_my_epa = plays %>%
  select(gameId, playId, playDescription, YardsFromOwnGoal,passResult, offensePlayResult,playResult, penaltyCodes,epa) %>%
  left_join(pbp_data_2018 %>%
              inner_join(my_epa_data) %>%
               ungroup() %>%
               select(gameId, playId, air_yards, yards_after_catch, my_epa))

plays_with_my_epa_check = plays_with_my_epa %>%
  filter(!is.na(my_epa))

cor(plays_with_my_epa_check$epa, plays_with_my_epa_check$my_epa)

# Compute EPA_throw For All Completions Using NNET Model ------------------

pass_attempt_caught_plays = pass_attempt_caught_preds %>%
  inner_join(pbp_data_2018 %>%
               inner_join(my_epa_data) %>%
               select(gameId, playId, TimeSecs_Remaining,
                      down, ydstogo, yrdline100,
                      air_yards, my_ep))

# fixing safeties and td's, and adjusting probabilities
pass_attempt_caught_plays2 = pass_attempt_caught_plays %>%
  mutate(yardline_end = yrdline100 - offensePlayResult) %>%
  mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                  yardline_end < 0 ~ 0,
                                  TRUE ~ as.numeric(yardline_end))) %>%
  select(-offensePlayResult) %>%
  group_by_at(vars(-probability)) %>%
  summarize(probability = sum(probability))

# fixing the down/distance/posteam
pass_attempt_caught_plays3 = pass_attempt_caught_plays2 %>%
  ungroup() %>%
  mutate(down = as.integer(down)) %>%
  mutate(first_down_flag = if_else(yrdline100 - yardline_end >= ydstogo, 1, 0),
         turnover_on_downs = if_else((first_down_flag == 0)&(down == 4), 1, 0),
         down_new = if_else(turnover_on_downs|first_down_flag, 1, down + 1),
         offensePlayResult = yrdline100 - yardline_end,
         yardline_new = if_else(turnover_on_downs == 1, 100 - yardline_end, yardline_end),
         ydstogo_new = if_else(turnover_on_downs|first_down_flag, 
                               pmin(yardline_new, 10),
                               ydstogo - (yrdline100 - yardline_new)),
         ) %>%
  inner_join(pbp_data_2018 %>%
               group_by(gameId, game_half) %>%
               mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
               select(gameId, playId, TimeSecs_Remaining_new)
               ) %>%
  mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
  select(gameId, playId, turnover_on_downs, offensePlayResult,
         ends_with("_new"), my_ep, probability)

colnames(pass_attempt_caught_plays3) = gsub("_new", "", names(pass_attempt_caught_plays3))

pass_attempt_caught_plays3 = pass_attempt_caught_plays3 %>%
  rename(yrdline100 = yardline) %>%
  mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
  mutate(log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down)) %>%
  mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))

rm(pass_attempt_caught_plays)
rm(pass_attempt_caught_plays2)


pass_attempt_caught_epa = as.data.frame(predict(ep_model, pass_attempt_caught_plays3, type = "probs")) %>%
  mutate(my_ep_pass_attempt = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
         + 2*(Opp_Safety - Safety))

pass_attempt_caught_plays3$my_ep_pass_attempt = pass_attempt_caught_epa$my_ep_pass_attempt

pass_attempt_caught_plays4 = pass_attempt_caught_plays3 %>%
  mutate(my_ep_pass_attempt = case_when(yrdline100 == 0 ~ 7,
                               yrdline100 == 100 ~ -2,
                               TRUE ~ my_ep_pass_attempt)) %>%
  mutate(my_ep_pass_attempt = if_else(turnover_on_downs == 1, -my_ep_pass_attempt, my_ep_pass_attempt)) %>%
  mutate(my_ep_pass_attempt = my_ep_pass_attempt - my_ep)

pass_attempt_caught_plays5 = pass_attempt_caught_plays4 %>%
  group_by(gameId, playId) %>%
  summarize(ey_catch = sum(offensePlayResult*probability),
            epa_pass_attempt = sum(my_ep_pass_attempt*probability))

plays_with_my_epa2 = plays_with_my_epa %>%
  left_join(pass_attempt_caught_plays5) %>%
  left_join(pass_attempt_class_preds)

rm(pass_attempt_caught_plays3,
   pass_attempt_caught_plays4,
   pass_attempt_caught_plays5,
   pass_attempt_caught_preds,
   pass_attempt_caught_epa)

# Compute EPA_throw For All Incompletions Using NNET Model ----------------

pass_attempt_incomplete_plays = pass_attempt_class_preds %>%
  select(gameId, playId) %>%
  mutate(offensePlayResult = 0,
         probability = 1) %>%
  inner_join(pbp_data_2018 %>%
               inner_join(my_epa_data) %>%
               select(gameId, playId, TimeSecs_Remaining,
                      down, ydstogo, yrdline100,
                      air_yards, my_ep))

# fixing safeties and td's, and adjusting probabilities
pass_attempt_incomplete_plays2 = pass_attempt_incomplete_plays %>%
  mutate(yardline_end = yrdline100 - offensePlayResult) %>%
  mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                  yardline_end < 0 ~ 0,
                                  TRUE ~ as.numeric(yardline_end))) %>%
  select(-offensePlayResult) %>%
  group_by_at(vars(-probability)) %>%
  summarize(probability = sum(probability))

# fixing the down/distance/posteam
pass_attempt_incomplete_plays3 = pass_attempt_incomplete_plays2 %>%
  ungroup() %>%
  mutate(down = as.integer(down)) %>%
  mutate(first_down_flag = if_else(yrdline100 - yardline_end >= ydstogo, 1, 0),
         turnover_on_downs = if_else((first_down_flag == 0)&(down == 4), 1, 0),
         down_new = if_else(turnover_on_downs|first_down_flag, 1, down + 1),
         offensePlayResult = yrdline100 - yardline_end,
         yardline_new = if_else(turnover_on_downs == 1, 100 - yardline_end, yardline_end),
         ydstogo_new = if_else(turnover_on_downs|first_down_flag, 
                               pmin(yardline_new, 10),
                               ydstogo - (yrdline100 - yardline_new)),
  ) %>%
  inner_join(pbp_data_2018 %>%
               group_by(gameId, game_half) %>%
               mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
               select(gameId, playId, TimeSecs_Remaining_new)
  ) %>%
  mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
  select(gameId, playId, turnover_on_downs, offensePlayResult,
         ends_with("_new"), my_ep, probability)

colnames(pass_attempt_incomplete_plays3) = gsub("_new", "", names(pass_attempt_incomplete_plays3))

pass_attempt_incomplete_plays3 = pass_attempt_incomplete_plays3 %>%
  rename(yrdline100 = yardline) %>%
  mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
  mutate(log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down)) %>%
  mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))

rm(pass_attempt_incomplete_plays)
rm(pass_attempt_incomplete_plays2)


pass_attempt_incomplete_epa = as.data.frame(predict(ep_model, pass_attempt_incomplete_plays3, type = "probs")) %>%
  mutate(my_ep_pass_attempt = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
         + 2*(Opp_Safety - Safety))

pass_attempt_incomplete_plays3$my_ep_pass_attempt = pass_attempt_incomplete_epa$my_ep_pass_attempt

pass_attempt_incomplete_plays4 = pass_attempt_incomplete_plays3 %>%
  mutate(my_ep_pass_attempt = case_when(yrdline100 == 0 ~ 7,
                                        yrdline100 == 100 ~ -2,
                                        TRUE ~ my_ep_pass_attempt)) %>%
  mutate(my_ep_pass_attempt = if_else(turnover_on_downs == 1, -my_ep_pass_attempt, my_ep_pass_attempt)) %>%
  mutate(my_ep_pass_attempt = my_ep_pass_attempt - my_ep)

pass_attempt_incomplete_plays5 = pass_attempt_incomplete_plays4 %>%
  group_by(gameId, playId) %>%
  summarize(epa_pass_incomplete = sum(my_ep_pass_attempt*probability))

plays_with_my_epa3 = plays_with_my_epa2 %>%
  left_join(pass_attempt_incomplete_plays5)

rm(pass_attempt_incomplete_plays3,
   pass_attempt_incomplete_plays4,
   pass_attempt_incomplete_plays5,
   pass_attempt_incomplete_epa)

# Compute EPA_throw For All Interceptions Using NNET Model ----------------

pass_attempt_intercepted_plays = pass_attempt_intercepted_preds %>%
  inner_join(pbp_data_2018 %>%
               inner_join(my_epa_data) %>%
               select(gameId, playId, TimeSecs_Remaining,
                      down, ydstogo, yrdline100,
                      air_yards, my_ep))

# fixing safeties and td's, and adjusting probabilities
pass_attempt_intercepted_plays2 = pass_attempt_intercepted_plays %>%
  mutate(yardline_end = (100 - (yrdline100 - offensePlayResult))) %>%
  mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                  yardline_end < 0 ~ 0,
                                  TRUE ~ as.numeric(yardline_end))) %>%
  select(-offensePlayResult) %>%
  group_by_at(vars(-probability)) %>%
  summarize(probability = sum(probability))

# fixing the down/distance/posteam
pass_attempt_intercepted_plays3 = pass_attempt_intercepted_plays2 %>%
  ungroup() %>%
  mutate(down = as.integer(down)) %>%
  mutate(turnover = 1,
         down_new = 1,
         offensePlayResult = yardline_end + yrdline100 - 100,
         yardline_new = yardline_end,
         ydstogo_new = pmin(yardline_new, 10)
  ) %>%
  inner_join(pbp_data_2018 %>%
               group_by(gameId, game_half) %>%
               mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
               select(gameId, playId, TimeSecs_Remaining_new)
  ) %>%
  mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
  select(gameId, playId, turnover, offensePlayResult,
         ends_with("_new"), my_ep, probability)

colnames(pass_attempt_intercepted_plays3) = gsub("_new", "", names(pass_attempt_intercepted_plays3))

pass_attempt_intercepted_plays3 = pass_attempt_intercepted_plays3 %>%
  rename(yrdline100 = yardline) %>%
  mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
  mutate(log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down)) %>%
  mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))

rm(pass_attempt_intercepted_plays,
   pass_attempt_intercepted_plays2)

pass_attempt_intercepted_epa = as.data.frame(predict(ep_model, pass_attempt_intercepted_plays3, type = "probs")) %>%
  mutate(my_ep_int = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
         + 2*(Opp_Safety - Safety))

pass_attempt_intercepted_plays3$my_ep_int = pass_attempt_intercepted_epa$my_ep_int

pass_attempt_intercepted_plays4 = pass_attempt_intercepted_plays3 %>%
  mutate(my_ep_int = case_when(yrdline100 == 0 ~ 7,
                                 yrdline100 == 100 ~ -2,
                                 TRUE ~ my_ep_int)) %>%
  mutate(my_ep_int = if_else(turnover == 1, -my_ep_int, my_ep_int)) %>%
  mutate(my_epa_throw = my_ep_int - my_ep)

pass_attempt_intercepted_plays5 = pass_attempt_intercepted_plays4 %>%
  group_by(gameId, playId) %>%
  summarize(ey_int = sum(offensePlayResult*probability),
            epa_int = sum(my_ep_int*probability))

plays_with_my_epa4 = plays_with_my_epa3 %>%
  left_join(pass_attempt_intercepted_plays5)


# Saving Metrics -------------------------------------------------------

plays_with_my_epa5 = plays_with_my_epa4 %>%
  rename(epa_pass_caught = epa_pass_attempt) %>%
  mutate(epa_pass_attempt = epa_pass_caught*C_prob +
                            epa_pass_incomplete*I_prob +
                            epa_int*IN_prob)

plays_with_my_epa6 = plays_with_my_epa5 %>%
  filter(!is.na(epa_pass_attempt)) %>%
  select(gameId, playId, C_prob, I_prob, IN_prob, epa_pass_attempt)

write.csv(plays_with_my_epa6, "~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv", row.names = FALSE)
