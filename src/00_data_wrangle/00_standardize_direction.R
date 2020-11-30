# This script standardizes the direction of each play to be to the left.
# Additionally, it adjusts variables such as dir and o.

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
library(rootSolve)
library(modeest)


# Reading in The Data -----------------------------------------------------

pbp_data = read.csv("~/Desktop/CoverageNet/inputs/week1.csv", stringsAsFactors = FALSE)
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv", stringsAsFactors = FALSE)

# Standardizing the Direction of the Plays ---------------------------------

# identifying possession team at a play level
poss_plays = plays %>%
  inner_join(games) %>%
  mutate(TeamOnOffense = ifelse(possessionTeam == homeTeamAbbr, "home", "away")) %>%
  select(names(plays), TeamOnOffense)

# adding necessary columns to pbp_data
pbp_data2 = pbp_data %>%
  inner_join(poss_plays) %>%
  mutate(IsOnOffense = TeamOnOffense == team) %>%
  select(names(pbp_data), IsOnOffense,
         down, yardsToGo, yardlineNumber, defendersInTheBox, possessionTeam, yardlineSide)


pbp_data2 <- pbp_data2 %>% 
  mutate(ToLeft = playDirection == "left")

pbp_data3 <- pbp_data2 %>% 
  mutate(YardsFromOwnGoal = ifelse(yardlineSide == possessionTeam, 
                                   yardlineNumber, 50 + (50-yardlineNumber)), 
         YardsFromOwnGoal = ifelse(yardlineNumber == 50, 50, YardsFromOwnGoal),  
         x_std = ifelse(ToLeft, 120-x, x) - 10, ## Standardizes X
         y_std = ifelse(ToLeft, 160/3-y, y))    ## Standardized Y  

pbp_data3 <- pbp_data3 %>% 
  mutate(dir_std_1 = ifelse(ToLeft & dir < 90, dir + 360, dir), 
         dir_std_1 = ifelse(!ToLeft & dir > 270, dir - 360, dir_std_1), 
         dir_std_2 = ifelse(ToLeft, dir_std_1 - 180, dir_std_1))

pbp_clean = pbp_data3 %>%
  select(-x, -y, -dir) %>%
  rename(x = x_std,
         y = y_std,
         dir = dir_std_2) %>%
  select(names(pbp_data), IsOnOffense, YardsFromOwnGoal)


# Checking That It Worked on Sample Plays ---------------------------------

pbp_pass = pbp_clean %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

pbp_pass %>%
  filter(gameId == 2018090600) %>%
  filter(playId %in% sample(unique((pbp_pass %>%
                                     filter(gameId == 2018090600))$playId), 4)) %>% 
  filter(event == "pass_forward",
         !is.na(nflId)) %>%
  ggplot(aes(x, y, fill = IsOnOffense), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = IsOnOffense), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate") + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank()) +
  facet_wrap(~playId, scale = 'free')


# Replicating the Above for All Years -------------------------------------

setwd("~/Desktop/CoverageNet/inputs/")
files = dir()[startsWith(dir(), "week")]

for(file in files){
  # reading in the play
  pbp_data_iter = read.csv(file, stringsAsFactors = FALSE)
  
  # identifying possession team at a play level
  poss_plays = plays %>%
    inner_join(games) %>%
    mutate(TeamOnOffense = ifelse(possessionTeam == homeTeamAbbr, "home", "away")) %>%
    select(names(plays), TeamOnOffense)
  
  # adding necessary columns to pbp_data
  pbp_data2 = pbp_data_iter %>%
    inner_join(poss_plays) %>%
    mutate(IsOnOffense = TeamOnOffense == team) %>%
    select(names(pbp_data_iter), IsOnOffense,
           down, yardsToGo, yardlineNumber, defendersInTheBox, possessionTeam, yardlineSide)
  
  
  pbp_data2 <- pbp_data2 %>% 
    mutate(ToLeft = playDirection == "left")
  
  pbp_data3 <- pbp_data2 %>% 
    mutate(YardsFromOwnGoal = ifelse(yardlineSide == possessionTeam, 
                                     yardlineNumber, 50 + (50-yardlineNumber)), 
           YardsFromOwnGoal = ifelse(yardlineNumber == 50, 50, YardsFromOwnGoal),  
           x_std = ifelse(ToLeft, 120-x, x) - 10, ## Standardizes X
           y_std = ifelse(ToLeft, 160/3-y, y))    ## Standardized Y  
  
  pbp_data3 <- pbp_data3 %>% 
    mutate(dir_std_1 = ifelse(ToLeft & dir < 90, dir + 360, dir), 
           dir_std_1 = ifelse(!ToLeft & dir > 270, dir - 360, dir_std_1), 
           dir_std_2 = ifelse(ToLeft, dir_std_1 - 180, dir_std_1)) %>%
    mutate(o_std_1 = ifelse(ToLeft & o < 90, o + 360, o), 
           o_std_1 = ifelse(!ToLeft & o > 270, o - 360, o_std_1), 
           o_std_2 = ifelse(ToLeft, o_std_1 - 180, o_std_1))
  
  pbp_clean_iter = pbp_data3 %>%
    select(-x, -y, -dir, -o) %>%
    rename(x = x_std,
           y = y_std,
           dir = dir_std_2,
           o = o_std_2) %>%
    select(names(pbp_data_iter), IsOnOffense, YardsFromOwnGoal)
  
  write.csv(pbp_clean_iter, 
            paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                   file),
            row.names = FALSE)
  
}

