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
library(broom)


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
coverages_week1 = read.csv("~/Desktop/CoverageNet/inputs/coverages_week1.csv")
man_coverage = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")

divider_all_total = read.csv("~/Desktop/CoverageNet/explore/outputs/divider_all_data.csv")

divider_all_total = divider_all_total %>%
  left_join(man_coverage,
          by = c("gameId", "playId", "nflId_off")) %>%
  filter(is.na(nflId_def)|(nflId == nflId_def)) %>%
  dplyr::select(-nflId_def)

divider_all_total %>%
  distinct(gameId, playId, nflId, nflId_off) %>%
  summarize(count = n())

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

divider_all_total %>%
  ggplot() +
  geom_density(aes(x = outside_leverage), fill = "blue", alpha = .7)

divider_all_total %>%
  ggplot() +
  geom_density(aes(x = separation), fill = "blue", alpha = .7)

set.seed(8451)

routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

unique(routes$route)


# Quantifying Success Over All Routes -------------------------------------

# success as maximum epa over route within 3 seconds (or time of pass)
play_max_epa = divider_all_total %>%
  inner_join(epa_tracking_total,
             by = c("gameId", "playId", "frameId",
                    "nflId_off" = "targetNflId")) %>%
  group_by(gameId, playId, nflId, nflId_off) %>%
  summarize(max_epa = max(epa_pass_attempt),
            play_epa_at_end = epa_pass_attempt[length(epa_pass_attempt)])


# filtering to parts of play we want to keep ------------------------------

divider_all_total2 = divider_all_total %>%
  filter(x > x_off)

divider_all_total3 = divider_all_total2 %>%
  filter(receiver_x_disp >= 0,
         receiver_x_disp < 15) %>%
  filter(receiver_dist_from_sideline <= 12) %>%
  filter(abs(receiver_disp_towards_sideline) <= 7.5) %>%
  filter((receiver_x_disp >= abs(receiver_disp_towards_sideline))|
           (abs(receiver_disp_towards_sideline) < 5)) %>%
  filter(separation <= 5)

# keeping only plays that started at the snap!
plays_from_snap = divider_all_total3 %>%
  filter(receiver_disp_towards_sideline < 3,
         receiver_x_disp < 3) %>%
  distinct(gameId, playId, nflId, nflId_off)

# keeping only routes of interest
divider_all_total4 = divider_all_total3 %>%
  inner_join(plays_from_snap) %>%
  inner_join(routes,
            by = c("gameId", "playId", "nflId_off" = "nflId")) %>%
  filter(route %in% c("HITCH", "OUT", "GO", "SLANT",
                      "CORNER","IN","POST"))

divider_all_total4 %>%
  distinct(gameId, playId, nflId, nflId_off) %>%
  summarize(plays = n(),
            plays_per_game = n()/(16*16))

# aggregating the data
frame_divider_epa_target = divider_all_total4 %>%
  mutate(leverage_type = case_when(abs(outside_leverage) <  .25 ~ "FACE UP",
                                   outside_leverage > 0 ~ 'OUTSIDE LEVERAGE',
                                   TRUE ~ 'INSIDE LEVERAGE')) %>%
  inner_join(play_max_epa) %>%
  filter(receiver_dist_from_sideline >= 5)

frame_divider_epa_target_agg = frame_divider_epa_target %>%
  filter(abs(receiver_x_disp - 7.5) < 2.5) %>%
  mutate(distance_from_sideline_disc = floor(receiver_dist_from_sideline*4)/4 + .125) %>%
  group_by(leverage_type, distance_from_sideline_disc) %>%
  summarize(count = n(),
            avg_x_disp = mean(receiver_x_disp),
            avg_leverage = mean(outside_leverage),
            avg_separation = mean(separation),
            avg_epa_at_end = mean(play_epa_at_end),
            avg_inc_epa = mean(play_epa_at_end > 0))

frame_divider_epa_target_agg %>%
  ggplot() +
  geom_point(aes(x = distance_from_sideline_disc, y = avg_epa_at_end, color = leverage_type)) + 
  geom_line(aes(x = distance_from_sideline_disc, y = avg_epa_at_end, color = leverage_type))



divider_by_play = divider_all_total4 %>%
  mutate(leverage_type = case_when(abs(outside_leverage) <  .25 ~ "FACE UP",
                                   outside_leverage > 0 ~ 'OUTSIDE LEVERAGE',
                                   TRUE ~ 'INSIDE LEVERAGE')) %>%
  group_by(gameId, playId, nflId, nflId_off, leverage_type) %>%
  summarize(divider_outside_lev = min(receiver_dist_from_sideline),
            divider_inside_lev = max(receiver_dist_from_sideline),
            frames = n()) %>%
  ungroup()

# Adding in EPA Analysis --------------------------------------------------

plays_keep = divider_all_total %>%
  filter(separation <= 2) %>%
  distinct(gameId, playId, nflId, nflId_off)

# looking at each frame!
frame_divider_epa_target = divider_all_total %>%
  mutate(leverage_type = case_when(abs(outside_leverage) <  .1 ~ "FACE UP",
                                   outside_leverage > 0 ~ 'OUTSIDE LEVERAGE',
                                 TRUE ~ 'INSIDE LEVERAGE')) %>%
  left_join(epa_tracking_total %>%
               dplyr::select(gameId, playId, frameId, targetNflId, 
                             C_prob, epa_pass_attempt),
             by = c("gameId", "playId", "frameId", "nflId_off" = "targetNflId")) %>%
  inner_join(epa_tracking_total %>%
               distinct(gameId, playId, targetNflId),
             by = c("gameId", "playId", "nflId_off" = "targetNflId")) %>%
  group_by(gameId, playId) %>%
  group_by(gameId, playId, nflId, nflId_off) %>%
  mutate(epa_at_end = epa_pass_attempt[length(epa_pass_attempt)]) %>%
  arrange(gameId, playId, nflId, nflId_off, desc(frameId)) %>%
  mutate(max_epa_after = cummax(epa_pass_attempt)) %>%
  arrange(gameId, playId, nflId, nflId_off, frameId) %>%
  fill(max_epa_after, .direction = "up") %>%
  filter(!is.na(epa_at_end)) %>%
  filter(x > x_off) %>%
  left_join(routes,
            by = c("gameId", "playId", "nflId_off" = "nflId")) %>%
  filter(route %in% c("HITCH", "OUT", "GO", "SLANT",
                      "CORNER","IN","POST")) %>%
  filter(receiver_dist_from_sideline < 12,
         receiver_dist_from_sideline > 5) %>%
  filter(x > x_off) %>%
  filter(receiver_x_disp >= 0,
         receiver_x_disp < 15) %>%
  filter(receiver_dist_from_sideline <= 12) %>%
  filter(abs(receiver_disp_towards_sideline) <= 1) %>%
  filter((receiver_x_disp >= abs(receiver_disp_towards_sideline))|
           (abs(receiver_disp_towards_sideline) < 5)) %>%
  filter(separation <= 5) %>%
  inner_join(plays_from_snap) %>%
  inner_join(plays_keep)

# outside leverage has a tail of higher separations that we need to account for
# FIXED BY ABOVE!
frame_divider_epa_target %>%
  ggplot() +
  geom_density(aes(x = separation, fill = leverage_type), alpha = .7)
  
# check counts
frame_divider_epa_target %>%
  group_by(leverage_type) %>%
  summarize(count = n())

frame_divider_epa_target_agg = frame_divider_epa_target %>%
  mutate(distance_from_sideline_disc = floor(receiver_dist_from_sideline*4)/4 + .125) %>%
  group_by(leverage_type, distance_from_sideline_disc) %>%
  summarize(count = n(),
            avg_leverage = mean(outside_leverage),
            avg_receiver_x_disp = mean(receiver_x_disp),
            avg_separation = mean(separation),
            avg_epa_at_end = mean(epa_at_end),
            avg_max_epa_after = mean(max_epa_after),
            avg_inc_epa = mean(max_epa_after > 0))

frame_divider_epa_target_agg %>%
  ggplot() +
  geom_point(aes(x = distance_from_sideline_disc, y = avg_max_epa_after, color = leverage_type)) + 
  geom_line(aes(x = distance_from_sideline_disc, y = avg_max_epa_after, color = leverage_type))

route_frame_divider_epa_target_agg = frame_divider_epa_target %>%
  mutate(route_break = case_when(route %in% c("OUT", "CORNER") ~ "OUT-BREAKING",
                                route %in% c("IN", "POST", "SLANT") ~ "IN-BREAKING",
                                TRUE ~ "NO-BREAKING")) %>%
  mutate(distance_from_sideline_disc = floor(distance_from_sideline*4)/4 + .125) %>%
  group_by(route_break, leverage_type, distance_from_sideline_disc) %>%
  summarize(count = n(),
            avg_leverage = mean(outside_leverage),
            avg_separation = mean(separation),
            avg_epa_at_end = mean(epa_at_end),
            avg_max_epa_after = mean(max_epa_after),
            avg_inc_epa = mean(max_epa_after > 0))


route_frame_divider_epa_target_agg %>%
  ggplot() +
  geom_point(aes(x = distance_from_sideline_disc, y = avg_max_epa_after, color = leverage_type)) + 
  geom_line(aes(x = distance_from_sideline_disc, y = avg_max_epa_after, color = leverage_type)) + 
  facet_wrap(~route_break)

  
# Determine Leverage By Play ----------------------------------------------

divider_by_play = frame_divider_epa_target %>%
  group_by(gameId, playId, nflId, nflId_off, leverage_type) %>%
  summarize(divider_outside_lev = min(distance_from_sideline),
            divider_inside_lev = max(distance_from_sideline),
            max_epa_after_outside_lev = max_epa_after[which.min(distance_from_sideline)],
            max_epa_after_inside_lev = max_epa_after[which.max(distance_from_sideline)],
            frames = n()) %>%
  ungroup() %>%
  filter(leverage_type != "FACE UP") %>%
  mutate(divider = if_else(leverage_type == 'OUTSIDE LEVERAGE', 
                           divider_outside_lev,
                           divider_inside_lev),
         max_epa_after = if_else(leverage_type == 'OUTSIDE LEVERAGE', 
                                 max_epa_after_outside_lev,
                                 max_epa_after_inside_lev))

targeted_route_epa_by_leverage_divider = epa_by_leverage_divider %>%
  mutate(distance_from_sideline_disc = floor(divider) + .5) %>%
  group_by(leverage_type, distance_from_sideline_disc) %>%
  summarize(count = n(),
            avg_epa = mean(max_epa_after),
            perc_increase_epa = mean(max_epa_after > 0))

targeted_route_epa_by_leverage_divider %>%
  ggplot() +
  geom_point(aes(x = distance_from_sideline_disc, y = avg_epa, color = leverage_type)) +
  geom_smooth(aes(x = distance_from_sideline_disc, y = avg_epa, color = leverage_type),
              method = "lm", se = FALSE)


