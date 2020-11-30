# This script pulls data on all safeties that are in pass coverage.
# All variables from: https://arxiv.org/pdf/1906.11373.pdf are pulled,
# as well as orientation-based and movement correlation based variables.
# 
# QB spikes are thrown out
# EDIT: should we elimate plays without pass_forward? (these are passing
# plays with things like a shovel pass, which may exhibit more run-like
# behavior than pass-behavior)

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
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
coverages_week1 = read.csv("~/Desktop/CoverageNet/inputs/coverages_week1.csv")

# Tagging Events ----------------------------------------------------------


setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

pass_attempt_total = data.frame()
sack_total = data.frame()

for(file in files){


  pbp_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                             file))
  
  print(paste0(file, " read"))

  pbp_data = pbp_data %>%
    arrange(gameId, playId, frameId, nflId) %>%
    mutate(event_flag = if_else(event %in% c('ball_snap', 'pass_forward',
                                             'pass_arrived', 'qb_sack',
                                             'qb_strip_sack', 'pass_shovel'),
                                1, 0)) %>%
    group_by(gameId, playId) %>%
    mutate(pass_arrived_flag = max(event == 'pass_arrived')) %>%
    ungroup() %>%
    mutate(event_flag = if_else((pass_arrived_flag == 0) &
                                (event %in% c('pass_outcome_incomplete', 
                                              'pass_outcome_interception',
                                              'pass_outcome_caught')),
                                1, event_flag)) %>%
    group_by(gameId, playId) %>%
    mutate(time_period = cumsum((event_flag == 1) & ((lag(event_flag) == 0)|
                                                     (event != lag(event))))) %>%
    mutate(time_period = replace_na(time_period, 0))
  
  # getting rid of QB spikes
  pbp_data2 = pbp_data %>%
    anti_join(pbp_data %>%
                filter(event == 'qb_spike') %>%
                select(gameId, playId))
  
  # getting rid of error prone plays (approx 3 out of 1032)
  check = pbp_data2 %>%
    group_by(gameId, playId) %>%
    summarize(events = max(time_period)) %>%
    inner_join(plays %>%
                 select(gameId, playId, passResult))
  
  clean_plays = check %>%
    filter(events >= 2) %>%
    filter((events == 3)|(passResult == 'S')) %>%
    select(gameId, playId)
  
  pbp_data3 = pbp_data2 %>%
    inner_join(clean_plays) %>%
    select(-ends_with("_flag"))
  
  pbp_data3_corners = pbp_data3 %>%
    filter(position %in% c('SS', 'FS', 'S'))
  
  pbp_data3_offense = pbp_data3 %>%
    filter(IsOnOffense) %>%
    rename(x_off = x,
           y_off = y,
           dir_off = dir,
           o_off = o,
           nflId_off = nflId,
           s_off = s) %>%
    select(gameId, playId, frameId, nflId_off, ends_with("_off"))
  
  pbp_data3_defense = pbp_data3 %>%
    filter(!IsOnOffense) %>%
    rename(x_def = x,
           y_def = y,
           dir_def = dir,
           o_def = o,
           nflId_def = nflId) %>%
    select(gameId, playId, frameId, nflId_def, ends_with("_def"))
  
  # Deriving Key Variables --------------------------------------------------
  
  pbp_data_pre_agg = pbp_data3_corners %>%
    inner_join(pbp_data3_offense) %>%
    mutate(dir2 = if_else(dir < 0, dir + 360, dir),
           o2 = if_else(o < 0, o + 360, o),
           dir_off2 = if_else(dir_off < 0, dir_off + 360, dir_off),
           o_off2 = if_else(o_off < 0, o_off + 360, o_off)) %>%
    mutate(cb_off_angle = case_when((x_off > x) & (y_off > y) ~ 90 - atan((y_off - y)/(x_off - x))*180/pi,
                                    (x_off > x) & (y_off < y) ~ 90 + atan((y - y_off)/(x_off - x))*180/pi,
                                    (x_off < x) & (y_off < y) ~ 270 - atan((y - y_off)/(x - x_off))*180/pi,
                                    (x_off < x) & (y_off > y) ~ 270 + atan((y_off - y)/(x - x_off))*180/pi,
                                    (x_off < x) & (y_off == y) ~ 270,
                                    (x_off > x) & (y_off == y) ~ 90,
                                    (x_off == x) & (y_off > y) ~ 0,
                                    (x_off == x) & (y_off < y) ~ 180,
                                    TRUE ~ 0
                                    )) %>%
    mutate(cb_off_dist = sqrt((x - x_off)^2 +  (y - y_off)^2),
           cb_off_dir_diff = if_else(abs(dir2 - dir_off2) < 180,
                                     abs(dir2 - dir_off2),
                                     360 - abs(dir2 - dir_off2)),
           cb_off_o_diff = if_else(abs(cb_off_angle - o2) < 180,
                                   abs(cb_off_angle - o2),
                                   360 - abs(cb_off_angle - o2))) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    # NEW CODE TO ELIMINATE RUB ROUTE CONFUSION!!!! ---------------
    mutate(off_min_dist_flag = cb_off_dist == min(cb_off_dist),
           off_dist_less_5_flag = cb_off_dist < 5) %>%
    group_by(gameId, playId, frameId, nflId,off_dist_less_5_flag) %>%
    mutate(min_dir_diff_of_close_players = cb_off_dir_diff == min(cb_off_dir_diff)) %>%
    ungroup() %>%
    filter((min_dir_diff_of_close_players & off_dist_less_5_flag)|(off_min_dist_flag)) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    filter(off_min_dist_flag == min(off_min_dist_flag)) %>%
    # -------------------------------------------------------------
    # OLD CODE:
    # filter(cb_off_dist == min(cb_off_dist)) %>%
    # -------------------------------------------------------------
    inner_join(pbp_data3_defense) %>%
    filter(nflId != nflId_def) %>%
    mutate(def_off_dist = sqrt((x_def - x_off)^2 + (y_def - y_off)^2)) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    filter(def_off_dist == min(def_off_dist)) %>%
    select(-ends_with("_def"),
           -ends_with("2")) %>%
    inner_join(pbp_data3_defense) %>%
    filter(nflId != nflId_def) %>%
    mutate(cb_def_dist = sqrt((x_def - x)^2 + (y_def - y)^2)) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    filter(cb_def_dist == min(cb_def_dist)) %>%
    select(-ends_with("_def")) %>%
    mutate(ratio = cb_off_dist/def_off_dist) %>%
    mutate(o2 = if_else(o < 0, o + 360, o)) %>%
    mutate(los_o_diff = if_else(abs(270 - o2) < 180,
                                  abs(270 - o2),
                                  360 - abs(270 - o2)))
  
  # adding in 45 degree rotation to handle correlation issue with low sd in
  # one principal direction
  pbp_data_pre_agg = pbp_data_pre_agg %>%
    inner_join(pbp_data %>%
                 ungroup() %>%
                 filter(event == "ball_snap",
                        is.na(nflId)) %>%
                 distinct(gameId, playId, .keep_all = TRUE) %>%
                 select(gameId, playId, x, y) %>%
                 rename(x_snap = x,
                        y_snap = y)) %>%
    mutate(x_rot_45 = (x - x_snap)*cos(pi/4) - (y - y_snap)*sin(pi/4),
           y_rot_45 = (y - y_snap)*cos(pi/4) + (x - x_snap)*sin(pi/4),
           x_off_rot_45 = (x_off - x_snap)*cos(pi/4) - (y_off - y_snap)*sin(pi/4),
           y_off_rot_45 = (y_off - y_snap)*cos(pi/4) + (x_off - x_snap)*sin(pi/4))
  
  # Aggregating Over Each Section of Plays ----------------------------------
  
  pbp_data_agg = rbind(
    pbp_data_pre_agg %>%
    ungroup() %>%
    mutate(time_period_group = case_when(time_period == 0 ~ "time_0",
                                         time_period == 1 ~ "time_1",
                                         time_period == 2 ~ "time_2",
                                         TRUE ~ "ignore")),
    
    pbp_data_pre_agg %>%
    ungroup() %>%
    filter(time_period <= 1) %>%
    mutate(time_period_group = "time_0_1"),
    
    pbp_data_pre_agg %>%
      ungroup() %>%
      filter(time_period %in% c(1, 2)) %>%
      mutate(time_period_group = "time_1_2")
    )%>%
    group_by(gameId, playId, nflId, displayName, position,
             time_period_group) %>%
    summarize(var_x = sd(x)^2,
              var_y = sd(y)^2,
              speed_var = sd(s)^2,
              speed_mean = mean(s),
              off_var = sd(cb_off_dist)^2,
              def_var = sd(cb_def_dist)^2,
              off_mean = mean(cb_off_dist),
              def_mean = mean(cb_def_dist),
              off_dir_var = sd(cb_off_dir_diff)^2,
              off_dir_mean = mean(cb_off_dir_diff),
              rat_mean = mean(ratio),
              rat_var = sd(ratio)^2,
              off_o_var = sd(cb_off_o_diff)^2,
              off_o_mean = mean(cb_off_o_diff),
              facing_off_perc = mean(cb_off_o_diff < 90),
              los_o_var = sd(los_o_diff)^2,
              los_o_mean = mean(los_o_diff),
              facing_los_perc = mean(los_o_diff < 90),
              cor_x = replace_na(cor(x, x_off), 0),
              cor_y = replace_na(cor(y, y_off), 0),
              cor_s = replace_na(cor(s, s_off), 0),
              cor_avg_x_y = (replace_na(cor(x, x_off),0) + 
                             replace_na(cor(y, y_off),0))/2,
              cor_x_rot_45 = replace_na(cor(x_rot_45, x_off_rot_45), 0),
              cor_y_rot_45 = replace_na(cor(y_rot_45, y_off_rot_45), 0),
              cor_avg_x_y_rot_45 = (replace_na(cor(x_rot_45, x_off_rot_45),0) + 
                               replace_na(cor(y_rot_45, y_off_rot_45),0))/2) %>%
    filter(time_period_group != 'ignore') %>%
    mutate(cor_rot_best_x = if_else(cor_avg_x_y_rot_45 > cor_avg_x_y,
                                    cor_x_rot_45, cor_x),
           cor_rot_best_y = if_else(cor_avg_x_y_rot_45 > cor_avg_x_y,
                                    cor_y_rot_45, cor_y),
           cor_rot_best_x_y = pmax(cor_avg_x_y, cor_avg_x_y_rot_45)
           ) %>%
    select(-ends_with("_rot_45"))
  
  
  # Splitting into Pass Attempt/Sack Plays ----------------------------------
  
  sack_pbp_data_agg = pbp_data_agg %>%
    inner_join(plays %>%
                 filter(passResult == 'S') %>%
                 select(gameId, playId)) %>%
    filter(!grepl('2', time_period_group))
  
  pass_attempt_data_agg = pbp_data_agg %>%
    inner_join(plays %>%
                 filter(passResult != 'S') %>%
                 select(gameId, playId))
  
  
  # Adding in Event-Measured Variables --------------------------------------
  
  # need to add in:
  # 1) ratio at ball_snap, pass_attempt (or sack)
  # 2) los_o_diff at pass_attempt (or at sack time)
  # 3) off_mean at pass_attempt and mid pass attempt
  
  
  pbp_data_event_variables = pbp_data_pre_agg %>%
    group_by(gameId, playId, time_period) %>%
    filter(frameId == min(frameId)) %>%
    filter(time_period %in% c(1, 2)) %>%
    mutate(event = case_when(grepl("pass", event) ~ "pass_forward",
                             grepl("sack", event) ~ "qb_sack",
                             grepl("snap", event) ~ "ball_snap",
                             TRUE ~ "error")) 
  
  pbp_data_event_variables_midpoint = pbp_data_event_variables %>%
    group_by(gameId, playId) %>%
    summarize(frameId = ceiling((max(frameId) + min(frameId))/2))
  
  pbp_data_event_variables_midpoint_vals = pbp_data_pre_agg %>%
    inner_join(pbp_data_event_variables_midpoint) %>%
    ungroup() %>%
    select(gameId, playId, nflId, ratio, cb_off_dir_diff, cb_off_dist) %>%
    rename(ratio_between_ball_snap_and_pass_forward = ratio, 
           off_dir_diff_between_ball_snap_and_pass_forward = cb_off_dir_diff,
           off_mean_between_ball_snap_and_pass_forward = cb_off_dist)
  
  pbp_data_event_variables = pbp_data_event_variables %>%
    anti_join(pbp_data_event_variables %>%
                filter(event == 'error') %>%
                ungroup() %>%
                select(gameId, playId)) %>%
    ungroup() %>%
    select(gameId, playId, nflId, event, ratio, cb_off_dir_diff, los_o_diff, cb_off_dist) %>%
    distinct(gameId, playId, nflId, event, .keep_all = TRUE) %>%
    pivot_wider(names_from = event,
                values_from = c(ratio, cb_off_dir_diff, los_o_diff, cb_off_dist)) %>%
    inner_join(pbp_data_event_variables_midpoint_vals)
  
  
  # Creating Final Data Sets (Passes/Sacks) ---------------------------------
  
  # there are some nulls when an event is only one frame long!
  # shovel passes or tipped passes exhibit this behavior. We choose
  # to drop those plays.
  pass_attempt_data_agg2 = pass_attempt_data_agg %>%
    ungroup() %>%
    pivot_wider(names_from = time_period_group,
                values_from = var_x:cor_rot_best_x_y) %>%
    inner_join(pbp_data_event_variables %>%
                 select(-ends_with("_sack")))
  
  pass_attempt_data_agg3 = pass_attempt_data_agg2 %>%
    ungroup() %>%
    drop_na(speed_var_time_0, speed_var_time_1, speed_var_time_2, ratio_pass_forward) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1])) %>%
    select(week, gameId, playId, nflId, everything())
  
  sack_pbp_data_agg2 = sack_pbp_data_agg %>%
    ungroup() %>%
    pivot_wider(names_from = time_period_group,
                values_from = var_x:cor_rot_best_x_y) %>%
    inner_join(pbp_data_event_variables %>%
                 select(-ends_with("_pass_forward")))
  
  sack_pbp_data_agg3 = sack_pbp_data_agg2 %>%
    ungroup() %>%
    drop_na(speed_var_time_0, speed_var_time_1, ratio_qb_sack) %>%
    mutate(week = as.integer(str_split(str_split(file, "week")[[1]][2], ".csv")[[1]][1])) %>%
    select(week, gameId, playId, nflId, everything())
  

  # Adding in Correlation Based Variables -----------------------------------
  # getting the max correlation b/w cornerback and offensive player in
  # both the x and y directions in the pre, mid, and post phases
  
  cor_data = rbind(
    pbp_data3_corners %>%
      inner_join(pbp_data3_offense) %>%
      ungroup() %>%
      mutate(time_period_group = case_when(time_period == 0 ~ "time_0",
                                           time_period == 1 ~ "time_1",
                                           time_period == 2 ~ "time_2",
                                           TRUE ~ "ignore")),
    
    pbp_data3_corners %>%
      inner_join(pbp_data3_offense) %>%
      ungroup() %>%
      filter(time_period <= 1) %>%
      mutate(time_period_group = "time_0_1"),
    
    pbp_data3_corners %>%
      inner_join(pbp_data3_offense) %>%
      ungroup() %>%
      filter(time_period %in% c(1, 2)) %>%
      mutate(time_period_group = "time_1_2")) %>%
    arrange(gameId, playId, nflId, nflId_off, frameId) %>%
    group_by(gameId, playId, nflId, nflId_off, time_period_group) %>%
    summarize(x_cor = replace_na(cor(x, x_off),0),
              y_cor = replace_na(cor(y, y_off),0)) %>%
    mutate(cor_avg = (x_cor + y_cor)/2) %>%
    arrange(gameId, playId, nflId, time_period_group, desc(cor_avg)) %>%
    group_by(gameId, playId, nflId, time_period_group) %>%
    filter(cor_avg == max(cor_avg)) %>%
    ungroup() %>%
    distinct(gameId, playId, nflId, time_period_group, .keep_all = TRUE) %>%
    select(gameId, playId, nflId, time_period_group, x_cor, y_cor, cor_avg) %>%
    rename(best_cor_x = x_cor,
           best_cor_y = y_cor,
           best_cor_x_y_avg = cor_avg) %>%
    pivot_wider(names_from = "time_period_group",
                values_from = best_cor_x:best_cor_x_y_avg) %>%
    select(-ends_with("ignore"))

  pass_attempt_data_agg3 = pass_attempt_data_agg3 %>%
    inner_join(cor_data)

  sack_pbp_data_agg3 = sack_pbp_data_agg3 %>%
    inner_join(cor_data %>% select(-ends_with("_time_2"),
                                   -ends_with("time_1_2")))
  
  
  # appending dfs
  pass_attempt_total = rbind(pass_attempt_total, pass_attempt_data_agg3) %>%
    arrange(week, gameId, playId, nflId)
  
  sack_total = rbind(sack_total, sack_pbp_data_agg3) %>%
    arrange(week, gameId, playId, nflId)
  
  print("loop finished")
  
  # checking for nulls
  sum(is.na(pass_attempt_total))
  colSums(is.na(pass_attempt_total))
  
  pass_attempt_total2 = pass_attempt_total %>%
    filter(!is.na(ratio_pass_forward))
  sum(is.na(pass_attempt_total2))
  
  sum(is.na(sack_total))
  colSums(is.na(sack_total))
  
  sack_total2 = sack_total %>%
    filter(!is.na(ratio_qb_sack))
  
  sum(is.na(sack_total2))
  
  write.csv(pass_attempt_total2,
            "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_pass_attempts_man_zone_gmm_features.csv",
            row.names = FALSE)
  
  write.csv(sack_total2,
            "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_sacks_man_zone_gmm_features.csv",
            row.names = FALSE)
  
}

# checking that all weeks were accounted for
check_passes = pass_attempt_total %>%
  group_by(week) %>%
  summarize(count = n())

check_players = pass_attempt_total %>%
  group_by(displayName) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

check_sacks = sack_total %>%
  group_by(week) %>%
  summarize(count = n())

# checking for nulls
sum(is.na(pass_attempt_total))
colSums(is.na(pass_attempt_total))

pass_attempt_total2 = pass_attempt_total %>%
  filter(!is.na(ratio_pass_forward))
sum(is.na(pass_attempt_total2))

sum(is.na(sack_total))
colSums(is.na(sack_total))

sack_total2 = sack_total %>%
  filter(!is.na(ratio_qb_sack)) %>%
  mutate(ratio_ball_snap = as.numeric(ratio_ball_snap),
         # ratio_qb_snap = as.numeric(los_o_diff_qb_sack),
         los_o_diff_ball_snap = as.numeric(los_o_diff_ball_snap),
         # los_o_diff_qb_sack = as.numeric(los_o_diff_qb_sack))
  )

sum(is.na(sack_total2))



