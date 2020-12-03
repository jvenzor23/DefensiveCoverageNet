# This script pulls each player's number for a given game.
# The main use of this data is for identifying player penalties, since
# the number of the offending player is listed in the plays data set.

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

games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

player_numbers = data.frame()

for(file in files){
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))
  
  pt_data2 = pt_data %>%
    filter(!is.na(nflId)) %>%
    distinct(gameId, nflId, team, IsOnOffense, jerseyNumber)
  
  player_numbers = rbind(player_numbers, pt_data2)
  
}

player_numbers2 = player_numbers %>%
  inner_join(games) %>%
  mutate(possessionTeam = case_when(IsOnOffense & (team == "away") ~ visitorTeamAbbr,
                                    IsOnOffense & (team == "home") ~ homeTeamAbbr,
                                    !IsOnOffense & (team == "away") ~ homeTeamAbbr,
                                    !IsOnOffense & (team == "home") ~ visitorTeamAbbr),
         isOnDefense = !IsOnOffense) %>%
  dplyr::select(gameId, nflId, isOnDefense, possessionTeam, jerseyNumber) %>%
  arrange(gameId, possessionTeam, jerseyNumber)


write.csv(player_numbers2,
          "~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/player_numbers.csv",
          row.names = FALSE)
