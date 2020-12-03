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
wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments.csv")


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

man_coverage_tracking = epa_tracking_total %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(nflId_def, time_after_snap) %>%
  summarise(count = n(),
            avg_epa = mean(epa_pass_attempt),
            avg_comp_perc = mean(C_prob)) %>%
  anti_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count < 100) %>%
              distinct(nflId_def)) %>%
  arrange(nflId_def, time_after_snap)

full_probs = dsn(x = seq(0, 15, .1), xi=0, omega=sn_fit[2], alpha=sn_fit[3], tau=sn_fit[1], dp=NULL, log=FALSE)/10

full_probs_df = data.frame("time_after_snap" = seq(0, 15, .1),
                           "prob" = full_probs) %>%
  mutate(dummy = 1) %>%
  full_join(man_coverage_tracking %>%
              distinct(nflId_def) %>%
              mutate(dummy = 1)) %>%
  dplyr::select(-dummy)

man_coverage_tracking_min_vals = man_coverage_tracking %>%
  group_by(nflId_def) %>%
  filter(time_after_snap == min(time_after_snap)) %>%
  inner_join(full_probs_df %>%
               distinct(nflId_def, time_after_snap) %>%
               rename(time_after_snap2 = time_after_snap)) %>%
  filter(time_after_snap2 < time_after_snap) %>%
  dplyr::select(-time_after_snap) %>%
  rename(time_after_snap = time_after_snap2) %>%
  dplyr::select(names(man_coverage_tracking))


man_coverage_tracking_max_vals = man_coverage_tracking %>%
  group_by(nflId_def) %>%
  filter(time_after_snap == max(time_after_snap)) %>%
  inner_join(full_probs_df %>%
               distinct(nflId_def, time_after_snap) %>%
               rename(time_after_snap2 = time_after_snap)) %>%
  filter(time_after_snap2 > time_after_snap) %>%
  dplyr::select(-time_after_snap) %>%
  rename(time_after_snap = time_after_snap2) %>%
  dplyr::select(names(man_coverage_tracking))

man_coverage_tracking2 = rbind(man_coverage_tracking,
                               man_coverage_tracking_min_vals,
                               man_coverage_tracking_max_vals) %>%
  arrange(nflId_def, time_after_snap)

man_coverage_tracking_score = man_coverage_tracking2 %>%
  inner_join(full_probs_df) %>%
  group_by(nflId_def) %>%
  summarize(normalized_avg_epa = sum(avg_epa*prob),
            normalized_comp_perc = sum(avg_comp_perc*prob)) %>%
  inner_join(wr_db_man_matchups %>%
               group_by(nflId_def) %>%
               summarize(routes = n())) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  arrange(desc(normalized_avg_epa))


man_coverage_tracking_score2 = man_coverage_tracking2 %>%
  filter(time_after_snap == 2.5) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  arrange(desc(avg_epa))
  

# Looking at Max EPA over Each Interval Per Play --------------------------

max_man_coverage_tracking = epa_tracking_total %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, nflId_def) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(nflId_def, time_after_snap) %>%
  summarise(count = n(),
            avg_max_epa = mean(epa_pass_attempt_max)) %>%
  anti_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count < 125) %>%
              distinct(nflId_def)) %>%
  arrange(nflId_def, time_after_snap) %>%
  group_by(nflId_def) %>%
  mutate(max_time_after_snap = max(time_after_snap)) %>%
  ungroup()

max_man_coverage_tracking_min_vals = max_man_coverage_tracking %>%
  group_by(nflId_def) %>%
  filter(time_after_snap == min(time_after_snap)) %>%
  inner_join(full_probs_df %>%
               distinct(nflId_def, time_after_snap) %>%
               rename(time_after_snap2 = time_after_snap)) %>%
  filter(time_after_snap2 < time_after_snap) %>%
  dplyr::select(-time_after_snap) %>%
  rename(time_after_snap = time_after_snap2) %>%
  dplyr::select(names(max_man_coverage_tracking))


max_man_coverage_tracking_max_vals = max_man_coverage_tracking %>%
  group_by(nflId_def) %>%
  filter(time_after_snap == max(time_after_snap)) %>%
  inner_join(full_probs_df %>%
               distinct(nflId_def, time_after_snap) %>%
               rename(time_after_snap2 = time_after_snap)) %>%
  filter(time_after_snap2 > time_after_snap) %>%
  dplyr::select(-time_after_snap) %>%
  rename(time_after_snap = time_after_snap2) %>%
  dplyr::select(names(max_man_coverage_tracking))

max_man_coverage_tracking2 = rbind.data.frame(max_man_coverage_tracking,
                               max_man_coverage_tracking_min_vals,
                               max_man_coverage_tracking_max_vals) %>%
  arrange(nflId_def, time_after_snap)

max_man_coverage_tracking_score = max_man_coverage_tracking %>%
  inner_join(full_probs_df) %>%
  group_by(nflId_def) %>%
  summarize(normalized_avg_max_epa = sum(avg_max_epa*prob),
            max_time_after_snap = max(max_time_after_snap)) %>%
  inner_join(wr_db_man_matchups %>%
               group_by(nflId_def) %>%
               summarize(routes = n())) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  arrange(desc(normalized_avg_max_epa))


# Fitting a Monotonic Function to Each Player -----------------------------

fitted_max_man_coverage_tracking = max_man_coverage_tracking %>%
  ungroup() %>%
  filter(time_after_snap <= min(max_time_after_snap))
  
library(scam)

# Break up d by state, then fit the specified model to each piece and
# return a list
models <- plyr::dlply(fitted_max_man_coverage_tracking, "nflId_def", function(df) 
  scam(avg_max_epa ~ s(time_after_snap, k = 12, bs = "mpi"), data = df))

# Apply coef to each model and return a data frame
fit_by_player = plyr::ldply(models, fitted.values)

fit_by_player2 = fit_by_player %>%
  pivot_longer(cols = setdiff(names(fit_by_player), "nflId_def"),
               names_to = "time_after_snap_placeholder",
               values_to = "avg_max_epa_pred") %>%
  group_by(nflId_def) %>%
  mutate(time_after_snap = row_number()*.1 + .4) %>%
  dplyr::select(-time_after_snap_placeholder)

fitted_max_man_coverage_tracking2 = fitted_max_man_coverage_tracking %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(fit_by_player2 %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  inner_join(full_probs_df %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  group_by(nflId_def) %>%
  mutate(prob_norm = prob/sum(prob)) %>%
  group_by(nflId_def) %>%
  summarize(normalized_avg_max_epa = sum(avg_max_epa_pred*prob_norm)) %>%
  inner_join(wr_db_man_matchups %>%
               group_by(nflId_def) %>%
               summarize(routes = n())) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  arrange(desc(normalized_avg_max_epa))


# Adjusting for WR Skill --------------------------------------------------

wr_under_150_skill1 = epa_tracking_total %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  inner_join(wr_db_man_matchups %>%
               group_by(nflId_off) %>%
               summarize(count = n()) %>%
               filter(count <= 50) %>%
               distinct(nflId_off),
             by = c("targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(time_after_snap) %>%
  summarise(count = n(),
            avg_max_epa = mean(epa_pass_attempt_max)) %>%
  mutate(targetNflId = 1)

wr_skill1 = epa_tracking_total %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(targetNflId, time_after_snap) %>%
  summarise(count = n(),
            avg_max_epa = mean(epa_pass_attempt_max)) %>%
  anti_join(wr_db_man_matchups %>%
              group_by(nflId_off) %>%
              summarize(count = n()) %>%
              filter(count < 50) %>%
              distinct(nflId_off),
            by = c("targetNflId" = "nflId_off")) %>%
  arrange(targetNflId, time_after_snap) %>%
  group_by(targetNflId) %>%
  mutate(max_time_after_snap = max(time_after_snap)) %>%
  ungroup()

fitted_wr_skill1 = wr_skill1 %>%
  ungroup() %>%
  filter(time_after_snap <= min(max_time_after_snap))

# Break up d by state, then fit the specified model to each piece and
# return a list
models <- plyr::dlply(fitted_wr_skill1, "targetNflId", function(df) 
  scam(avg_max_epa ~ s(time_after_snap, k = 12, bs = "mpi"), data = df))

# Apply coef to each model and return a data frame
fit_by_wr = plyr::ldply(models, fitted.values)

fit_by_wr2 = fit_by_wr %>%
  pivot_longer(cols = setdiff(names(fit_by_wr), "targetNflId"),
               names_to = "time_after_snap_placeholder",
               values_to = "avg_max_epa_pred") %>%
  group_by(targetNflId) %>%
  mutate(time_after_snap = row_number()*.1 + .4) %>%
  dplyr::select(-time_after_snap_placeholder)

fitted_wr_skill2 = fitted_wr_skill1 %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(fit_by_wr2 %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  inner_join(full_probs_df %>%
               distinct(time_after_snap, prob) %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  group_by(targetNflId) %>%
  mutate(prob_norm = prob/sum(prob)) %>%
  group_by(targetNflId) %>%
  summarize(normalized_avg_max_epa = sum(avg_max_epa_pred*prob_norm)) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId=nflId_off) %>%
               group_by(targetNflId) %>%
               summarize(routes = n())) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName),
             by = c("targetNflId" = "nflId")) %>%
  arrange(desc(normalized_avg_max_epa))

# Break up d by state, then fit the specified model to each piece and
# return a list
models <- plyr::dlply(wr_under_150_skill1, "targetNflId", function(df) 
  scam(avg_max_epa ~ s(time_after_snap, k = 12, bs = "mpi"), data = df))

# Apply coef to each model and return a data frame
fit_by_wr_under_150 = plyr::ldply(models, fitted.values)

fit_by_wr_under_150_2 = fit_by_wr_under_150 %>%
  pivot_longer(cols = setdiff(names(fit_by_wr_under_150), "targetNflId"),
               names_to = "time_after_snap_placeholder",
               values_to = "avg_max_epa_pred") %>%
  group_by(targetNflId) %>%
  mutate(time_after_snap = row_number()*.1 + .4) %>%
  dplyr::select(-time_after_snap_placeholder)

wr_under_150_skill2 = wr_under_150_skill1 %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(fit_by_wr_under_150_2 %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  inner_join(full_probs_df %>%
               distinct(time_after_snap, prob) %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  group_by(targetNflId) %>%
  mutate(prob_norm = prob/sum(prob)) %>%
  group_by(targetNflId) %>%
  summarize(normalized_avg_max_epa = sum(avg_max_epa_pred*prob_norm))

db_matchup_difficulty = epa_tracking_total %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  anti_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count < 125) %>%
              distinct(nflId_def) %>%
              rename()) %>%
  distinct(gameId, playId, targetNflId, nflId_def) %>%
  left_join(fitted_wr_skill2 %>%
              dplyr::select(-routes, -displayName))

db_matchup_difficulty$normalized_avg_max_epa[is.na(db_matchup_difficulty$normalized_avg_max_epa)] = wr_under_150_skill2$normalized_avg_max_epa

db_matchup_difficulty2 = db_matchup_difficulty %>%
  group_by(nflId_def) %>%
  summarize(wr_normalized_avg_max_epa = mean(normalized_avg_max_epa))

db_matchup_difficulty3 = fitted_max_man_coverage_tracking2 %>%
  inner_join(db_matchup_difficulty2) %>%
  mutate(eps = wr_normalized_avg_max_epa - normalized_avg_max_epa) %>%
  arrange(desc(eps))
