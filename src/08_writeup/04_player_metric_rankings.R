# This code animates a given play in the player tracking data, while
# also displaying the epa values for all receivers (targeted receiver
# shown in red)

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
library(tidytext)

# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")

overall_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_overall.csv",
                           check.names = FALSE)
man_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_man.csv",
                       check.names = FALSE)
zone_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_zone.csv",
                        check.names = FALSE)

team_images = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_team_info.csv")
playerInfo = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_player_info.csv")

playerTeams = playerInfo %>%
  distinct(nflId, team)

# Vizualizing EPS and SOS EPS (top 15) ------------------------------------

mytheme <- theme_minimal() +
  theme(axis.text.y = element_text(hjust=0),
        strip.text = element_text(size=13))

library(ggimage)
library(ggtext)
overall_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  dplyr::select(Player, `Pass Snaps`, EPS,`SOS EPS`, teamImageUrl) %>%
  rename(`Strengh-of-Schedule Adjusted EPS` = `SOS EPS`) %>%
  pivot_longer(cols = EPS:`Strengh-of-Schedule Adjusted EPS`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(as.character(row_number()), 
                         ". ",
                         as.character(Player))) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "grey", color = "black") + 
  geom_image(aes(x = reorder_within(Player, value, metric), image = teamImageUrl), y = 0,  # add geom_image layer
            size = 0.05) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  labs(y = "Points Saved",
       x = "Player Name (Team)") +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free")

