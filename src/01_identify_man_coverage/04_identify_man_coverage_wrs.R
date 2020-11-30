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
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")

man_zone_classification = rbind(
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_pass_attempts_man_zone_classes.csv") %>%
    select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/corners_sacks_man_zone_classes.csv") %>%
    select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_pass_attempts_man_zone_classes.csv") %>%
    select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/safeties_sacks_man_zone_classes.csv") %>%
    select(gameId, playId, nflId, zone_probability)
) %>%
  arrange(gameId, playId, nflId)


# Filtering to Players In Man Coverage ------------------------------------

# we require at least a 90% man probability to consider a player in man coverage
man_coverage = man_zone_classification %>%
  filter(zone_probability <= .1)


# Deriving Attributes to Determine DB-WR pairs ----------------------------

# 1) correlation of motion from ball_snap to pass_forward (or sack)
# 2) average separation from ball_snap to pass_forward (or sack)
# 3) separation at pass_forward (or sack)
# 4) distance from QB at pass_forward (or sack) to determine if player rushed

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

matchups_final_write = data.frame()

for(file in files){
  
  
  pbp_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                             file))

  pbp_data_man_plays = pbp_data %>%
    inner_join(man_coverage %>%
                 distinct(gameId, playId))
  
  pbp_data_man_plays2 = pbp_data_man_plays %>%
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
  pbp_data_man_plays3 = pbp_data_man_plays2 %>%
    anti_join(pbp_data_man_plays2 %>%
                filter(event == 'qb_spike') %>%
                select(gameId, playId))
  
  # getting rid of error prone plays (approx 3 out of 1032)
  check = pbp_data_man_plays3 %>%
    group_by(gameId, playId) %>%
    summarize(events = max(time_period)) %>%
    inner_join(plays %>%
                 select(gameId, playId, passResult))
  
  clean_plays = check %>%
    filter(events >= 2) %>%
    filter((events == 3)|(passResult == 'S')) %>%
    select(gameId, playId)
  
  pbp_data_man_plays4 = pbp_data_man_plays3 %>%
    inner_join(clean_plays) %>%
    select(-ends_with("_flag"))
  
  pbp_data_dbs_in_man = pbp_data_man_plays4 %>%
    inner_join(man_coverage %>%
                 distinct(gameId, playId, nflId)) %>%
    filter(time_period == 1)
  
  pbp_data_offense = pbp_data_man_plays4 %>%
    filter(IsOnOffense,
           time_period == 1) %>%
    rename(x_off = x,
           y_off = y,
           dir_off = dir,
           o_off = o,
           nflId_off = nflId,
           s_off = s,
           position_off = position) %>%
    select(gameId, playId, frameId, nflId_off, ends_with("_off"))
  
  pbp_data_pre_agg = pbp_data_dbs_in_man %>%
    inner_join(pbp_data_offense) %>%
    mutate(dist = sqrt((x - x_off)^2 +  (y - y_off)^2)) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(last_frame = frameId == max(frameId)) %>%
    mutate(dist_last_frame = if_else(last_frame, dist, 0)) %>%
    ungroup() %>%
    inner_join(pbp_data_man_plays4 %>%
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
           y_off_rot_45 = (y_off - y_snap)*cos(pi/4) + (x_off - x_snap)*sin(pi/4)) %>%
    group_by(gameId, playId, nflId, nflId_off, position_off) %>%
    summarize(movement_cor = (cor(x, x_off) + cor(y, y_off))/2,
              movement_cor2 = (cor(x_rot_45, x_off_rot_45) + cor(y_rot_45, y_off_rot_45))/2,
              avg_sep = mean(dist),
              sep_at_end = max(dist_last_frame)) %>%
    mutate(movement_cor = pmax(movement_cor, movement_cor2)) %>%
    select(-movement_cor2)
  
  
  qb_data_pre_agg = pbp_data_pre_agg %>%
    filter(position_off == 'QB')
  
  non_qb_data_pre_agg = pbp_data_pre_agg %>%
    filter(position_off != 'QB')
  
  # requiring greater than .6 correlation, and keeping the player with the lowest
  # avg sep
  pbp_data_agg = non_qb_data_pre_agg %>%
    filter((movement_cor > .6)|(avg_sep < 10)) %>%
    group_by(gameId, playId, nflId) %>%
    filter(movement_cor > 0,
           (avg_sep < 10)|(movement_cor > .95),
           avg_sep < 15) %>%
    # logic to say, elimiate something if cor is way worse, with sep at end not much
    # better
    mutate(max_cor = max(movement_cor),
           max_cor_sep_at_end = sep_at_end[which.max(movement_cor)]) %>%
    mutate(cor_reduction = (movement_cor - max_cor)/(max_cor),
           sep_at_end_improve = (max_cor_sep_at_end - sep_at_end)/max_cor_sep_at_end) %>%
    filter((cor_reduction > -.25)|(sep_at_end_improve > .50)) %>%
    filter(avg_sep == min(avg_sep)) 
  
  # checking if they rushed
  pbp_data_agg2 = pbp_data_agg %>%
    inner_join(qb_data_pre_agg %>%
                ungroup() %>%
                select(gameId, playId, nflId, sep_at_end) %>%
                rename(qb_sep_at_end = sep_at_end), 
              by = c("gameId", "playId", "nflId")) %>%
    filter(qb_sep_at_end > sep_at_end,
           qb_sep_at_end > 5)
  
  # final df
  matchups_final = pbp_data_agg2 %>%
    select(gameId, playId, nflId, nflId_off) %>%
    rename(nflId_def = nflId) %>%
    distinct(gameId, playId, nflId_def, .keep_all = TRUE)
  
  matchups_final_write = rbind.data.frame(matchups_final_write,
                               matchups_final)
  
  write.csv(matchups_final_write,
            "~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments.csv",
            row.names = FALSE)
  
}

# counting most common man defenders in the data set (gives an idea of eventual
# sample size)

def_players_count = matchups_final_write %>%
  group_by(nflId_def) %>%
  summarize(man_count = n()) %>%
  arrange(desc(man_count)) %>%
  inner_join(players %>%
               select(nflId, displayName, position), by = c("nflId_def" = "nflId")) %>%
  select(displayName, position, everything())


# Looking at epa with targeted reciever -----------------------------------

epa_when_targeted = matchups_final_write %>%
  inner_join(targeted_receiver) %>%
  mutate(targeted = targetNflId == nflId_off) %>%
  inner_join(plays %>% select(gameId, playId, epa)) %>%
  group_by(nflId_def) %>%
  summarize(man_count = n(),
            perc_targeted = mean(1*targeted, na.rm = TRUE),
            epa_when_targeted = mean(if_else(targeted, epa, as.numeric(NA)), na.rm = TRUE)) %>%
  arrange(desc(man_count)) %>%
  inner_join(players %>%
               select(nflId, displayName, position), by = c("nflId_def" = "nflId")) %>%
  select(displayName, position, everything()) %>%
  filter(man_count >= 100)


