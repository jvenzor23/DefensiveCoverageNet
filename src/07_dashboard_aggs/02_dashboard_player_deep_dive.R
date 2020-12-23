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


# Reading in The Data -----------------------------------------------------

overall_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_overall.csv",
                           check.names=FALSE)
man_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_man.csv",
                       check.names=FALSE)
zone_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_zone.csv",
                        check.names=FALSE)

players = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_player_info.csv")

# Man And Zone Plots Single Player ----------------------------------------

# normal metrics
man_normal = man_ratings %>%
  dplyr::select(nflId_def, `Man EPS Tracking`,`Man EPS Closing`,
                `Man EPS Ball Skills`, `Man EPS Tackling`,
                `Man EPS Ball Hawk`,`Man EPS INT Returns`) %>%
  pivot_longer(cols = starts_with("Man"),
               names_to = "Metric",
               values_to = "EPS") %>%
  mutate(Metric = gsub("Man EPS ", "", Metric)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName) %>%
               rename(nflId_def = nflId)) %>%
  mutate(`Coverage Type` = "Man")

zone_normal = zone_ratings %>%
  dplyr::select(nflId_def,`Zone EPS Closing`,
                `Zone EPS Ball Skills`, `Zone EPS Tackling`,
                `Zone EPS Ball Hawk`,`Zone EPS INT Returns`) %>%
  pivot_longer(cols = starts_with("Zone"),
               names_to = "Metric",
               values_to = "EPS") %>%
  mutate(Metric = gsub("Zone EPS ", "", Metric)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName) %>%
               rename(nflId_def = nflId)) %>%
  mutate(`Coverage Type` = "Zone")

tot_normal = rbind(man_normal,
                   zone_normal) %>%
  arrange(nflId_def, Metric)

# example plot
example.plot.man = tot_normal %>%
  filter(nflId_def == 2558067,
         `Coverage Type` == "Man")

ggplot() +
  geom_bar(data = example.plot.man %>%
             mutate(Metric = as.factor(Metric)) %>%
             mutate(Metric = fct_reorder(Metric, EPS)) %>%
             mutate(above_0_flag = EPS > 0) %>%
             arrange(desc(above_0_flag)),
           aes(x = EPS,
               y = Metric,
               fill = above_0_flag), 
           stat = "identity",
           width = .65) +
  labs(x = "Expected Points Saved",
       y = "Pass Defense Component",
       title = paste0("Man Pass Defense Skill Breakdown For ",
                      unique(example.plot$displayName))) +
  theme_minimal() +
  theme(legend.position = "none")

# example plot
example.plot.zone = tot_normal %>%
  filter(nflId_def == 2558176,
         `Coverage Type` == "Zone")

ggplot() +
  geom_bar(data = example.plot.zone %>%
             mutate(Metric = as.factor(Metric)) %>%
             mutate(Metric = fct_reorder(Metric, EPS)) %>%
             mutate(above_0_flag = EPS > 0) %>%
             arrange(desc(above_0_flag)),
           aes(x = EPS,
               y = Metric,
               fill = above_0_flag), 
           stat = "identity",
           width = .65) +
  labs(x = "Expected Points Saved",
       y = "Pass Defense Component",
       title = paste0("Zone Pass Defense Skill Breakdown For ",
                      unique(example.plot$displayName))) +
  theme_minimal() +
  theme(legend.position = "none")

