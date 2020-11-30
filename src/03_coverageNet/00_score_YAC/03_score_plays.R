# This code translates the yardage/INT predictions into epa.

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
# 2018 Completions YAC distribtions
yac_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_predictions_completions.csv")
# 2018 Interception YAINT distribtions
yaint_preds = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yaint_predictions_interceptions.csv")
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

yac_plays = yac_preds %>%
  inner_join(pbp_data_2018 %>%
               inner_join(my_epa_data) %>%
               select(gameId, playId, TimeSecs_Remaining,
                      down, ydstogo, yrdline100,
                      air_yards, my_ep))

# fixing safeties and td's, and adjusting probabilities
yac_plays2 = yac_plays %>%
  mutate(yardline_end = yrdline100 - air_yards - yards_after_catch) %>%
  mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                  yardline_end < 0 ~ 0,
                                  TRUE ~ as.numeric(yardline_end))) %>%
  select(-yards_after_catch) %>%
  group_by_at(vars(-probability)) %>%
  summarize(probability = sum(probability))

# fixing the down/distance/posteam
yac_plays3 = yac_plays2 %>%
  ungroup() %>%
  mutate(down = as.integer(down)) %>%
  mutate(first_down_flag = if_else(yrdline100 - yardline_end >= ydstogo, 1, 0),
         turnover_on_downs = if_else((first_down_flag == 0)&(down == 4), 1, 0),
         down_new = if_else(turnover_on_downs|first_down_flag, 1, down + 1),
         yards_after_catch = yrdline100 - air_yards - yardline_end,
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
  select(gameId, playId, turnover_on_downs, yards_after_catch,
         ends_with("_new"), my_ep, probability)

colnames(yac_plays3) = gsub("_new", "", names(yac_plays3))

yac_plays3 = yac_plays3 %>%
  rename(yrdline100 = yardline) %>%
  mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
  mutate(log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down)) %>%
  mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))


yac_epa = as.data.frame(predict(ep_model, yac_plays3, type = "probs")) %>%
  mutate(my_ep_throw = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
         + 2*(Opp_Safety - Safety))

yac_plays3$my_ep_throw = yac_epa$my_ep_throw

yac_plays4 = yac_plays3 %>%
  mutate(my_ep_throw = case_when(yrdline100 == 0 ~ 7,
                               yrdline100 == 100 ~ -2,
                               TRUE ~ my_ep_throw)) %>%
  mutate(my_ep_throw = if_else(turnover_on_downs == 1, -my_ep_throw, my_ep_throw)) %>%
  mutate(my_epa_throw = my_ep_throw - my_ep)

yac_plays5 = yac_plays4 %>%
  group_by(gameId, playId) %>%
  summarize(eyac = sum(yards_after_catch*probability),
            epa_throw = sum(my_epa_throw*probability))

plays_with_my_epa2 = plays_with_my_epa %>%
  left_join(yac_plays5) %>%
  mutate(epa_yac = my_epa - epa_throw,
         ey_throw = air_yards + eyac)


# fixing touchdown passes in the endzone
# for these, we want to assign epa_throw to be my_epa,
# epa_yac to be null, eyac as null, ey_throw as playResult
plays_with_my_epa2 = plays_with_my_epa2 %>%
  mutate(epa_throw = if_else(((100 - YardsFromOwnGoal) == air_yards) &
                               (air_yards == playResult),
                             my_epa,
                             epa_throw),
         ey_throw = if_else(((100 - YardsFromOwnGoal) == air_yards) &
                               (air_yards == playResult),
                            as.numeric(playResult),
                            ey_throw))

completions_check = plays_with_my_epa2 %>%
  filter(passResult == "C",
         penaltyCodes == "")

# plays not included are due to no pass_outcome_caught in data
mean(!is.na(completions_check$epa_throw))

# filling in incompletions
plays_with_my_epa3 = plays_with_my_epa2 %>%
  mutate(ey_throw = if_else(passResult == 'I', 0, ey_throw),
         epa_throw = if_else(passResult == 'I', my_epa, epa_throw))

# Compute EPA_throw For All Interceptions Using NNET Model ----------------

yaint_plays = yaint_preds %>%
  inner_join(pbp_data_2018 %>%
               inner_join(my_epa_data) %>%
               select(gameId, playId, TimeSecs_Remaining,
                      down, ydstogo, yrdline100,
                      air_yards, my_ep))

# fixing safeties and td's, and adjusting probabilities
yaint_plays2 = yaint_plays %>%
  mutate(yardline_end = (100 - (yrdline100 - air_yards)) - yards_after_catch) %>%
  mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                  yardline_end < 0 ~ 0,
                                  TRUE ~ as.numeric(yardline_end))) %>%
  select(-yards_after_catch) %>%
  group_by_at(vars(-probability)) %>%
  summarize(probability = sum(probability))

# fixing the down/distance/posteam
yaint_plays3 = yaint_plays2 %>%
  ungroup() %>%
  mutate(down = as.integer(down)) %>%
  mutate(turnover = 1,
         down_new = 1,
         yards_after_catch = (100 - (yrdline100 - air_yards)) - yardline_end,
         yardline_new = yardline_end,
         ydstogo_new = pmin(yardline_new, 10)
  ) %>%
  inner_join(pbp_data_2018 %>%
               group_by(gameId, game_half) %>%
               mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
               select(gameId, playId, TimeSecs_Remaining_new)
  ) %>%
  mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
  select(gameId, playId, turnover, yards_after_catch,
         ends_with("_new"), my_ep, probability)

colnames(yaint_plays3) = gsub("_new", "", names(yaint_plays3))

yaint_plays3 = yaint_plays3 %>%
  rename(yrdline100 = yardline) %>%
  mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
  mutate(log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down)) %>%
  mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))


yaint_epa = as.data.frame(predict(ep_model, yaint_plays3, type = "probs")) %>%
  mutate(my_ep_throw = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
         + 2*(Opp_Safety - Safety))

yaint_plays3$my_ep_throw = yaint_epa$my_ep_throw

yaint_plays4 = yaint_plays3 %>%
  mutate(my_ep_throw = case_when(yrdline100 == 0 ~ 7,
                                 yrdline100 == 100 ~ -2,
                                 TRUE ~ my_ep_throw)) %>%
  mutate(my_ep_throw = if_else(turnover == 1, -my_ep_throw, my_ep_throw)) %>%
  mutate(my_epa_throw = my_ep_throw - my_ep)

yaint_plays5 = yaint_plays4 %>%
  group_by(gameId, playId) %>%
  summarize(eyaint = sum(yards_after_catch*probability),
            epa_int = sum(my_epa_throw*probability))

plays_with_my_epa4 = plays_with_my_epa3 %>%
  left_join(yaint_plays5) %>%
  mutate(epa_yaint = my_epa - epa_int) %>%
  mutate(epa_throw = if_else(!is.na(epa_int), epa_int, epa_throw)) %>%
  select(-epa_int)


# QAing YAC Metrics -------------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv", stringsAsFactors = FALSE)
targetedReciever = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv", stringsAsFactors = FALSE)

catches_not_in_endzone = plays_with_my_epa4 %>%
  filter(passResult == 'C',
         !is.na(eyac)) %>%
  inner_join(targetedReciever) %>%
  inner_join(players, by = c("targetNflId" = "nflId"))

best_yac_players = catches_not_in_endzone %>%
  group_by(targetNflId, displayName) %>%
  summarize(RECs = n(),
            YAC = sum(yards_after_catch),
            eYAC = sum(eyac),
            addedYACperRec = (YAC - eYAC)/RECs,
            YAC_epa = sum(epa_yac),
            YAC_epaPerRec = mean(epa_yac),
            pctAddedYAC = mean(yards_after_catch > eyac),
            pctAddedEPA = mean(epa_yac > 0),
            EPA_added = sum(my_epa)) %>%
  arrange(desc(RECs)) %>%
  filter(RECs >= 50)

check = plays %>%
  filter(penaltyCodes %in% c("", "OPI", "DPI"))


# Saving the Data ---------------------------------------------------------

plays_with_my_epa5 = plays_with_my_epa4 %>%
  select(gameId, playId, my_epa, epa_throw, epa_yac,
         eyac, ey_throw, eyaint, epa_yaint) %>%
  rename(epa = my_epa)

write.csv(plays_with_my_epa5, "~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv", row.names = FALSE)
