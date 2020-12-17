# This code scores players closing ability
# RULES:
# 1) all plays where a player commits a penalty during the tracking phase are excluded
#    from scoring based on our epa tracking values, and will be assinged the negative
#    epa value associated with the penalty
# -- this includes defensive holding, illegal contact, and illegal use of hands
# 2) any accepted penalty will be held against a player, while any declined
#    or offsetting penalty will not be held against a player (but that player
#    will not be evaluated based on their tracking with epa values for that
#    play!)
# 3) Unfortunately, plays with accepted penalties do not include routes.
#    As a result, the valid data on all non-penalized players are not
#    included in this analysis. To fix, we would update the 00_get_tracking_pt_data.R
#    function called in 00_score_plays.R to include these players (although
#    we still would not know their route without doing much more work to build
#    a classification algorithm based on assigned routes)

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/CoverageNet/inputs/")

# Calling Necessary Libraries
library(tidyverse)
# library(dplyr)
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

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv", stringsAsFactors = FALSE)
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv", stringsAsFactors = FALSE)
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
playerImages = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/playerImages.csv", stringsAsFactors = FALSE)
collegeImages = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_college_info.csv", stringsAsFactors = FALSE)

# cleaning names
players2 = players %>%
  mutate(displayNameJoin = str_to_lower(str_replace_all(trimws(gsub("(II)|(III)|(Jr.)|(Sr.)", "", displayName)), "[[:punct:]]", "")))

playerImages2 = playerImages %>%
  mutate(displayNameJoin = str_to_lower(str_replace_all(trimws(gsub("(II)|(III)|(Jr.)|(Sr.)", "", displayName)), "[[:punct:]]", ""))) %>%
  rename(collegeNameESPN = collegeName,
         positionESPN = position,
         displayNameESPN = displayName)

# Getting Colleges --------------------------------------------------------

college_names_links = players_with_images = players2 %>%
  left_join(playerImages2) %>%
  mutate(positionDisc = case_when(grepl("LB", position)|(position %in% c("DE", "DT", "NT")) ~ "DL_LB",
                                  position %in% c("DB", "CB", 'SS', 'FS', 'S') ~ "DB",
                                  position %in% c("HB", "RB", "FB", "WR") ~ "OFF_SKILL",
                                  TRUE ~ position),
         positionESPNDisc = case_when(grepl("LB", positionESPN)|(positionESPN %in% c("DE", "DT", "NT")) ~ "DL_LB",
                                      positionESPN %in% c("DB", "CB", 'SS', 'FS', 'S') ~ "DB",
                                      positionESPN %in% c("HB", "RB", "FB", "WR") ~ "OFF_SKILL",
                                      positionESPN == "PK" ~ "K",
                                      TRUE ~ positionESPN)) %>%
  filter((positionDisc == positionESPNDisc)|(displayName %in% c("Tremon Smith", "Jordan Franks"))) %>%
  distinct(collegeName, collegeNameESPN) %>%
  arrange(desc(collegeName)) %>%
  filter(!((collegeName == "North Dakota State") & (collegeNameESPN == "Utah")),
         !((collegeName == "Louisiana State") & (collegeNameESPN == "LSU-Shreveport")),
         !((collegeName == "Georgia Tech") & (collegeNameESPN == "South Carolina")))
  

# Joining Player Images to Players ----------------------------------------


players_with_images = players2 %>%
  left_join(playerImages2) %>%
  mutate(positionDisc = case_when(grepl("LB", position)|(position %in% c("DE", "DT", "NT")) ~ "DL_LB",
                                  position %in% c("DB", "CB", 'SS', 'FS', 'S') ~ "DB",
                                  position %in% c("HB", "RB", "FB", "WR") ~ "OFF_SKILL",
                                  TRUE ~ position),
         positionESPNDisc = case_when(grepl("LB", positionESPN)|(positionESPN %in% c("DE", "DT", "NT")) ~ "DL_LB",
                                      positionESPN %in% c("DB", "CB", 'SS', 'FS', 'S') ~ "DB",
                                      positionESPN %in% c("HB", "RB", "FB", "WR") ~ "OFF_SKILL",
                                      positionESPN == "PK" ~ "K",
                                  TRUE ~ positionESPN)
         ) %>%
  filter((positionDisc == positionESPNDisc)|(displayName %in% c("Tremon Smith", "Jordan Franks"))) %>%
  dplyr::select(displayName, nflId, height, weight, birthDate, collegeName, position,
         team, playerImageUrl) %>%
  arrange(displayName)

check = players_with_images %>%
  group_by(nflId) %>%
  summarize(count = n())


# Practice Plotting -------------------------------------------------------

# library(ggimage)
# dbs_with_images = players_with_images %>%
#   filter(position %in% c("DB", "CB")) %>%
#   rowwise() %>%
#   mutate(height = if_else(grepl("-", height),
#                           as.numeric(str_split(height, "-")[[1]][1])*12 +
#                             as.numeric(str_split(height, "-")[[1]][2]),
#                           as.numeric(height)))
# 
# ggplot() +
#   geom_image(data = dbs_with_images %>%
#                mutate(playerImageUrl = as.factor(playerImageUrl)) %>%
#                mutate(playerImageUrl = fct_reorder(playerImageUrl, desc(weight))),
#              aes(x = height, y = weight, 
#                  image=playerImageUrl), size=.1, alpha = 1)


# Pulling Player Teams ----------------------------------------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

players_teams_tot = data.frame()
players_plays_tot = data.frame()

for(file in files){
  
  pbp_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file),
                      stringsAsFactors = FALSE)

  players_teams = pbp_data %>%
    filter(!is.na(nflId)) %>%
    inner_join(games) %>%
    mutate(team_name = if_else(team == "away", visitorTeamAbbr, homeTeamAbbr)) %>%
    distinct(gameId, nflId, displayName, team_name)
  
  players_plays = pbp_data %>%
    filter(!is.na(nflId)) %>%
    distinct(nflId, gameId, playId)
  
  players_teams_tot = rbind(players_teams_tot,
                            players_teams)
  
  players_plays_tot = rbind(players_plays_tot,
                            players_plays)
  
}

players_teams_final = players_teams_tot %>%
  group_by(nflId, displayName, team_name) %>%
  summarize(games = n()) %>%
  ungroup() %>%
  arrange(displayName) %>%
  group_by(nflId) %>%
  mutate(team_number = row_number()) %>%
  pivot_wider(names_from = team_number,
              values_from = c(team_name, games)) %>%
  mutate(games = sum(games_1, games_2, na.rm = TRUE)) %>%
  mutate(team = if_else(is.na(team_name_2),
                        team_name_1,
                        paste0(team_name_1, "/", team_name_2))) %>%
  dplyr::select(nflId, displayName, team, games)

players_plays_final = players_plays_tot %>%
  distinct(nflId, gameId, playId) %>%
  group_by(nflId) %>%
  summarize(plays = n())

# summarizing the final player_df
player_df_final = players_teams_final %>%
  dplyr::select(-displayName) %>%
  inner_join(players_plays_final) %>%
  left_join(players_with_images %>%
              dplyr::select(nflId, playerImageUrl)) %>%
  left_join(players %>%
              dplyr::select(displayName, nflId, height, weight, birthDate, collegeName, position)) %>%
  dplyr::select(displayName, everything()) %>%
  left_join(college_names_links) %>%
  mutate(collegeNameESPN = if_else(!is.na(collegeNameESPN),
                                   collegeNameESPN,
                                   collegeName)) %>%
  dplyr::select(-collegeName) %>%
  rename(collegeName = collegeNameESPN) %>%
  filter(!grepl("NA", team))


write.csv(player_df_final, "~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_player_info.csv")


# Getting Team Info -------------------------------------------------------

teams = games %>%
  distinct(homeTeamAbbr) %>%
  rename(teamName = homeTeamAbbr) %>%
  mutate(teamImageUrl = paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/nfl/500/",
                               str_to_lower(teamName),
                               ".png&w=40&h=40&cquality=40&scale=crop&location=origin&transparent=true")) %>%
  mutate(division = case_when(teamName %in% c('NE', 'NYJ', 'BUF', 'MIA') ~ 'AFC East',
                              teamName %in% c('CLE', 'PIT', 'BAL', 'CIN') ~ 'AFC North',
                              teamName %in% c('IND', 'JAX', 'TEN', 'HOU') ~ 'AFC South',
                              teamName %in% c('DEN', 'KC', 'OAK', 'LAC') ~ 'AFC West',
                              teamName %in% c('DAL', 'WAS', 'NYG', 'PHI') ~ 'NFC East',
                              teamName %in% c('CHI', 'GB', 'DET', 'MIN') ~ 'NFC North',
                              teamName %in% c('ATL', 'CAR', 'NO', 'TB') ~ 'NFC South',
                              teamName %in% c('SEA', 'SF', 'ARI', 'LA') ~ 'NFC West',
                              TRUE ~ 'ERROR')) %>%
  mutate(conference = str_split(division, " ")[[1]][1]) %>%
  arrange(division)

write.csv(teams, "~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_team_info.csv")

