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
library(magick)
library(fitdistrplus)
library(skewt)
library(sn)
library(broom)
library(dplyr)

# Reading in the data -----------------------------------------------------

# man metrics
man_tracking = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashboard_player_tracking_eps_plays_viz.csv") %>%
  mutate(coverage = "man",
         metric = "tracking") %>%
  dplyr::select(-week) %>%
  mutate(play_type = NA_character_)
man_closing = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashbaord_player_closing_eps_plays_viz.csv") %>%
  mutate(coverage = "man",
         metric = "closing") %>%
  rename(eps = eps_closing) %>%
  mutate(time_after_snap = NA_real_,
         play_type = NA_character_)
man_ball_skills = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashbaord_player_ball_skills_eps_plays_viz.csv")%>%
  mutate(coverage = "man",
         metric = "ball_skills") %>%
  rename(eps = eps_ball_skills) %>%
  mutate(time_after_snap = NA_real_,
         play_type = NA_character_)
man_tackling = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashbaord_player_tackling_eps_plays_viz.csv")%>%
  mutate(coverage = "man",
         metric = "tackling") %>%
  rename(eps = eps_tackling) %>%
  mutate(targetNflId = NA_integer_,
         time_after_snap = NA_real_,
         play_type = NA_character_)
man_int_returns = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashbaord_player_int_returns_eps_plays_viz.csv")%>%
  mutate(coverage = "man",
         metric = "int_returns") %>%
  rename(eps = eps_yaint) %>%
  mutate(targetNflId = NA_integer_,
         time_after_snap = NA_real_,
         play_type = NA_character_)
man_ball_hawk = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashbaord_player_ball_hawk_eps_plays_viz.csv")%>%
  mutate(coverage = "man",
         metric = "ball_hawk") %>%
  rename(eps = eps_ball_hawk) %>%
  mutate(targetNflId = NA_integer_,
         time_after_snap = NA_real_,
         play_type = NA_character_)

# zone metrics
zone_closing = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_closing_eps_plays_viz.csv")%>%
  mutate(coverage = "zone",
         metric = "closing") %>%
  mutate(time_after_snap = NA_real_,
         play_type = NA_character_) %>%
  rename(eps = eps_closing)
zone_ball_skills = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_ball_skills_eps_plays_viz.csv")%>%
  mutate(coverage = "zone",
         metric = "ball_skills") %>%
  mutate(time_after_snap = NA_real_,
         play_type = NA_character_) %>%
  rename(eps = eps_ball_skills)
zone_tackling = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_tackling_eps_plays_viz.csv")%>%
  mutate(coverage = "zone",
         metric = "tackling") %>%
  mutate(targetNflId = NA_integer_,
         time_after_snap = NA_real_,
         play_type = NA_character_) %>%
  rename(eps = eps_tackling)
zone_int_returns = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_int_returns_eps_plays_viz.csv")%>%
  mutate(coverage = "zone",
         metric = "int_returns") %>%
  mutate(targetNflId = NA_integer_,
         time_after_snap = NA_real_,
         play_type = NA_character_) %>%
  rename(eps = eps_yaint)
zone_ball_hawk = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_ball_hawk_eps_plays_viz.csv")%>%
  mutate(coverage = "zone",
         metric = "ball_hawk") %>%
  rename(eps = eps_ball_hawk) %>%
  mutate(targetNflId = NA_integer_,
         time_after_snap = NA_real_)


# Aggregating the data ----------------------------------------------------

data_tot = rbind(man_tracking,
                 man_closing,
                 man_ball_skills,
                 man_tackling,
                 man_ball_hawk,
                 man_int_returns,
                 zone_closing,
                 zone_ball_skills,
                 zone_tackling,
                 zone_ball_hawk,
                 zone_int_returns) %>%
  dplyr::select(-play_type)

write.csv(data_tot,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_animate_best_worst_plays/player_animate_best_worst_plays.csv",
          row.names = FALSE)
