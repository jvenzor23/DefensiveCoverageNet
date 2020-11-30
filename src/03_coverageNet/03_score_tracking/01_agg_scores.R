# This code investigates aggregations of the player tracking epa data. 

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
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
my_epa_data = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")
epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

epa_tracking_total %>%
  distinct(week, gameId, playId, targetNflId, frameId) %>%
  group_by(week) %>%
  summarize(count = n())

epa_tracking_total %>%
  distinct(week, gameId, playId) %>%
  group_by(week) %>%
  summarize(count = n())

epa_tracking_total %>%
  distinct(week, gameId) %>%
  group_by(week) %>%
  summarize(count = n())

epa_tracking_total %>%
  anti_join(plays %>%
               filter(passResult %in% c('S')) %>%
               select(gameId, playId)) %>%
  distinct(week, gameId, playId) %>%
  group_by(week) %>%
  summarize(count = n())


# Checking Average Routes Per Play ----------------------------------------

routes = epa_tracking_total %>%
  distinct(gameId, playId, targetNflId) %>%
  group_by(gameId, playId) %>%
  summarize(receivers = n())

mean(routes$receivers)

# Aggregating Metrics -----------------------------------------------------
epa_tracking_total_time_agg = epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(time_after_snap) %>%
  summarize(routes_count = n(),
            avg_e_comp_perc = mean(C_prob),
            avg_epa = mean(epa_pass_attempt)) %>%
  filter(time_after_snap <= 7) %>%
  arrange(time_after_snap)

max_epa_tracking_total_time_agg = epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(time_after_snap) %>%
  filter(time_after_snap <= 8) %>%
  summarize(routes_count = n(),
            avg_max_epa = mean(epa_pass_attempt_max)) %>%
  arrange(time_after_snap)

max_epa_tracking_total_time_agg %>%
  ggplot() +
  geom_point(aes(x = time_after_snap, y = avg_max_epa)) +
  xlim(0, 8) +
  ylim(0, 1)

epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(time_after_snap) %>%
  filter(time_after_snap == 6.5) %>%
  ggplot() +
  geom_density(aes(x = epa_pass_attempt_max), fill = "blue", alpha = .5) +
  xlim(-4, 4)

epa_tracking_best_time_agg = epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, time_after_snap) %>%
  summarize(best_comp_perc = max(C_prob),
            best_epa = max(epa_pass_attempt)) %>%
  group_by(time_after_snap) %>%
  summarize(plays_count = n(),
            avg_best_comp_perc = mean(best_comp_perc),
            avg_best_epa = mean(best_epa)) %>%
  filter(time_after_snap <= 7) %>%
  arrange(time_after_snap)


max_best_epa_tracking_total_time_agg = epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, time_after_snap) %>%
  summarize(best_epa = max(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  mutate(best_epa_pass_attempt_max = cummax(best_epa)) %>%
  ungroup() %>%
  group_by(time_after_snap) %>%
  filter(time_after_snap <= 8) %>%
  summarize(routes_count = n(),
            avg_best_epa_max = mean(best_epa_pass_attempt_max)) %>%
  arrange(time_after_snap)

max_best_epa_tracking_total_time_agg %>%
  ggplot() +
  geom_point(aes(x = time_after_snap, y = avg_best_epa_max)) +
  xlim(0, 8) +
  ylim(0, 2)

epa_at_pass_attempt_metrics = epa_tracking_total %>%
  inner_join(plays %>%
               filter(passResult %in% c('C','I','INT')) %>%
               select(gameId, playId)) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, frameId) %>%
  mutate(best_comp_at_frame = C_prob == max(C_prob),
         best_epa_at_frame = epa_pass_attempt == max(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(best_comp_at_some_frame = max(best_comp_at_frame),
         best_epa_at_frame = max(best_epa_at_frame)) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  filter(frameId == max(frameId)) %>%
  inner_join(targeted_receiver %>%
               rename(actualTargetNflId = targetNflId)) %>%
  filter(!is.na(actualTargetNflId)) %>%
  group_by(gameId, playId) %>%
  mutate(highest_comp = C_prob == max(C_prob),
         highest_epa = epa_pass_attempt == max(epa_pass_attempt),
         highest_comp_targeted = (actualTargetNflId == targetNflId) & highest_comp,
         highest_comp_at_some_point_targeted = (actualTargetNflId == targetNflId) & best_comp_at_some_frame,
         highest_epa_targeted = (actualTargetNflId == targetNflId) & highest_epa,
         highest_epa_at_some_point_targeted = (actualTargetNflId == targetNflId) & best_epa_at_frame) %>%
  group_by(gameId, playId) %>%
  summarize(time_after_snap = time_after_snap[1],
            best_c_prob = max(C_prob),
            best_c_prob_targeted = max(1*highest_comp_targeted),
            best_c_prob_at_some_point_targeted = max(1*highest_comp_at_some_point_targeted),
            best_epa = max(epa_pass_attempt),
            best_epa_targeted = max(1*highest_epa_targeted),
            best_epa_at_some_point_targeted = max(1*highest_epa_at_some_point_targeted)
            ) %>%
  group_by(time_after_snap) %>%
  summarize(count = n(),
            avg_best_comp_perc = mean(best_c_prob),
            perc_best_comp_perc_targeted = mean(best_c_prob_targeted),
            perc_best_comp_per_at_some_point_targeted = mean(best_c_prob_at_some_point_targeted),
            avg_best_epa = mean(best_epa),
            perc_best_epa_targeted = mean(best_epa_targeted),
            perc_best_epa_at_some_point_targeted = mean(best_epa_at_some_point_targeted)) %>%
  mutate(plays_perc = count/sum(count)) %>%
  select(time_after_snap, count, plays_perc, everything()) %>%
  filter(count > 10)


# Checking the Time of Pass Attempt Metrics -------------------------------

pass_attempt_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv")

check = epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  filter(frameId == max(frameId)) %>%
  inner_join(targeted_receiver) %>%
  left_join(pass_attempt_epa_data %>%
              select(gameId, playId, epa_pass_attempt) %>%
              rename(epa = epa_pass_attempt))

