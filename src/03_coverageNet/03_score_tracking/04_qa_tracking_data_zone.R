# This script begins to look at player tracking ability by defender,
# but is more a QA. A comprehensive look into scoring player tracking
# ability is considered in the next section (still needs to be done!).

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
library(magick)
library(fitdistrplus)
library(skewt)
library(sn)
library(broom)

# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")
wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments2.csv")
wr_db_zone_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments.csv")

# Analyzing Distribution of Throw Times -----------------------------------

time_to_throw_dist = epa_tracking_total %>%
  anti_join(plays %>%
              filter(passResult == 'S') %>%
              distinct(gameId, playId)) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  summarize(time_to_throw = max(time_after_snap))

sn_fit = fitdist(time_to_throw_dist$time_to_throw,
               distr = "sn",
               method = "mle",
               start = list("tau" = 0, "omega" = 2.75, "alpha" = 3))$estimate

time_to_throw_dist2 = time_to_throw_dist %>%
  group_by(time_to_throw) %>%
  summarize(count = n()) %>%
  mutate(perc = count/sum(count))

sn_fitted_vals = dsn(x = time_to_throw_dist2$time_to_throw, xi=0, omega=sn_fit[2], alpha=sn_fit[3], tau=sn_fit[1], dp=NULL, log=FALSE)

time_to_throw_dist2$fitted_perc = sn_fitted_vals/10

time_to_throw_dist2 %>%
  ggplot() +
  geom_density(stat = "identity", aes(x = time_to_throw, y = fitted_perc),
               fill = "blue", alpha = .6) +
  geom_density(stat = "identity", aes(x = time_to_throw, y = perc)) +
  labs(x = "Time To Pass Attempt(s)",
       y = "Probability Density")


# Rating Defensive Backs --------------------------------------------------

zone_epa_tracking_total_time_agg = epa_tracking_total %>%
  inner_join(wr_db_zone_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  arrange(gameId, playId, targetNflId, nflId, frameId) %>%
  group_by(gameId, playId, targetNflId, nflId, frameId_start) %>%
  mutate(time_after_entering_zone = ((frameId - min(frameId)))*.1) %>%
  ungroup() %>%
    group_by(time_after_entering_zone) %>%
    summarize(routes_count = n(),
              avg_e_comp_perc = mean(C_prob),
              avg_epa = mean(epa_pass_attempt)) %>%
    filter(time_after_entering_zone <= 5) %>%
    arrange(time_after_entering_zone)

man_epa_tracking_total_time_agg = epa_tracking_total %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(time_after_snap) %>%
  summarize(routes_count = n(),
            avg_e_comp_perc = mean(C_prob),
            avg_epa = mean(epa_pass_attempt)) %>%
  filter(time_after_snap <= 5) %>%
  arrange(time_after_snap)

ggplot() +
  geom_line(data = man_epa_tracking_total_time_agg,
            aes(x = time_after_snap, y = avg_epa), color = "blue") +
  geom_line(data = zone_epa_tracking_total_time_agg,
           aes(x = time_after_entering_zone, y = avg_epa), color = "red")
  
  

