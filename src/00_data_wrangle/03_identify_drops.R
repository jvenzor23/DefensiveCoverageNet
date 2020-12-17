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

# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")



# Finding Drops -----------------------------------------------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

drops_tot = data.frame()

for(file in files){

  pbp_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  pbp_data_incompletions = pbp_data %>%
    inner_join(plays %>% 
                 filter(passResult == "I") %>%
                 distinct(gameId, playId)) %>%
    filter(event == "pass_arrived")
  
  incompletions_off_target = plays %>% 
                 inner_join(pbp_data %>%
                              distinct(gameId, playId)) %>%
                 filter(passResult == "I") %>%
                 distinct(gameId, playId) %>%
                 anti_join(pbp_data_incompletions %>%
                             distinct(gameId, playId))
  
  pbp_data_targeted_receiver = pbp_data_incompletions %>%
    inner_join(targeted_receiver,
               by = c("gameId", "playId", "nflId" = "targetNflId"))
  
  pbp_data_football = pbp_data_incompletions %>%
    filter(is.na(nflId)) %>%
    dplyr::select(gameId, playId, frameId, x, y) %>%
    rename(x_football = x,
           y_football = y)
  
  pbp_data_defense = pbp_data_incompletions %>%
    filter(!IsOnOffense,
           !is.na(nflId)) %>%
    dplyr::select(gameId, playId, frameId, x, y)
  
  receiver_close_enough = pbp_data_targeted_receiver %>%
    inner_join(pbp_data_football) %>%
    mutate(receiver_football_dist = sqrt((x - x_football)^2 + (y - y_football)^2)) %>%
    filter(receiver_football_dist <= 1.5,
           y > 0, 
           y < 53 + 1/3,
           x > 0,
           x < 120)
  
  defenders_close_enough = pbp_data_defense %>%
    inner_join(pbp_data_football) %>%
    mutate(receiver_football_dist = sqrt((x - x_football)^2 + (y - y_football)^2)) %>%
    filter(receiver_football_dist <= 1.5) %>%
    distinct(gameId, playId)
  
  defenders_close_enough_to_receiver = pbp_data_targeted_receiver %>%
    inner_join(pbp_data_defense %>%
                 rename(x_def = x,
                        y_def = y)) %>%
    mutate(receiver_defender_dist = sqrt((x - x_def)^2 + (y - y_def)^2)) %>%
    filter(receiver_defender_dist <= 1.5) %>%
    distinct(gameId, playId)
  
  drops_plays = receiver_close_enough %>%
    anti_join(defenders_close_enough) %>%
    anti_join(defenders_close_enough_to_receiver) %>%
    distinct(gameId, playId, nflId, displayName)
  
  drops_tot = rbind(drops_plays,
                    drops_tot)
  
}

# QA ----------------------------------------------------------------------

player_drops = drops_tot %>% 
  group_by(displayName, nflId) %>% 
  summarize(drops = n()) %>%
  arrange(desc(drops))

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  dplyr::select(play_id, game_id, interception_player_name, 
                pass_defense_1_player_name,
                pass_defense_2_player_name) %>%
  rename(gameId = game_id,
         playId = play_id)

pbus = pbp_data_2018 %>%
  filter(!is.na(pass_defense_1_player_name))

# should be 0!
drops_and_pbu = drops_tot %>%
  inner_join(pbus)

drops_clean = drops_tot %>%
  anti_join(drops_and_pbu) %>%
  distinct(gameId, playId)

write.csv(drops_clean,
            "~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/drops.csv",
          row.names = FALSE)
