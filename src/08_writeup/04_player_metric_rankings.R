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
library(ggtext)

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
  distinct(nflId, playerImageUrl, team, displayName)

# Vizualizing EPS and SOS EPS (top 15) ------------------------------------

mytheme <- theme_minimal() +
  theme(# axis.text.y = element_text(hjust=0),
        plot.title = element_text(hjust = 0),
        strip.text = element_text(size=13),
        axis.text.y = element_markdown(color = "black", size = 11, hjust = 0))

library(ggimage)
library(ggtext)
overall_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(`Pass Snaps` > 100) %>%
  dplyr::select(Player, `Pass Snaps`, EPS,`SOS EPS`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  rename(`Strengh-of-Schedule Adjusted EPS` = `SOS EPS`) %>%
  pivot_longer(cols = EPS:`Strengh-of-Schedule Adjusted EPS`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
                         # ". ",
                         "<img src='",
                         playerImageUrl,
                         "' width='20' />",
                         " ",
                         as.character(row_number()),
                         ". ", 
                         as.character(displayName),
                         " <img src='",
                         as.character(teamImageUrl),
                         "' height='15' />"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free_y") +
  labs(y = "Points Saved",
       x = "Player Name (Team)",
       title = "Overall Metrics")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/OverallScores.png",
       height = 4,
       width = 12)
# Zone Vs. Man Metrics ----------------------------------------------------

combined_ratings = man_ratings %>%
  full_join(zone_ratings, 
            by = c("nflId_def", "Pos", "Player"))
# overall
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x + Covers.y > 100) %>%
  dplyr::select(Player, `Man EPS`,`Zone EPS`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS`:`Zone EPS`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/OverallScoresManZone.png",
       height = 4,
       width = 12)

# tracking
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x > 100) %>%
  dplyr::select(Player, `Man EPS Tracking`,`SOS Man EPS Tracking`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS Tracking`:`SOS Man EPS Tracking`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
    mutate(Player = paste0(# as.character(row_number()), 
      # ". ",
      "<img src='",
      playerImageUrl,
      "' width='20' />",
      " ",
      as.character(row_number()),
      ". ", 
      as.character(displayName),
      " <img src='",
      as.character(teamImageUrl),
      "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/TrackingScores.png",
       height = 4,
       width = 12)

# closing
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x + Covers.y > 100) %>%
  dplyr::select(Player, `Man EPS Closing`,`Zone EPS Closing`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS Closing`:`Zone EPS Closing`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/ClosingScores.png",
       height = 4,
       width = 12)

# ball skills
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x + Covers.y > 100) %>%
  dplyr::select(Player, `Man EPS Ball Skills`,`Zone EPS Ball Skills`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS Ball Skills`:`Zone EPS Ball Skills`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/BallSkillsScores2.png",
       height = 4,
       width = 12)

# tackling
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x + Covers.y > 100) %>%
  dplyr::select(Player, `Man EPS Tackling`,`Zone EPS Tackling`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS Tackling`:`Zone EPS Tackling`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/TacklingScores.png",
       height = 4,
       width = 12)

# Ball Hawk
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x + Covers.y > 100) %>%
  dplyr::select(Player, `Man EPS Ball Hawk`,`Zone EPS Ball Hawk`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS Ball Hawk`:`Zone EPS Ball Hawk`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/BallHawkScores2.png",
       height = 4,
       width = 12)

# INT Returns
combined_ratings %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
  filter(Covers.x + Covers.y > 100) %>%
  dplyr::select(Player, `Man EPS INT Returns`,`Zone EPS INT Returns`, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  pivot_longer(cols = `Man EPS INT Returns`:`Zone EPS INT Returns`,
               names_to = "metric",
               values_to = "value") %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' top:20px/>"))%>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, value, metric),
                   xend=reorder_within(Player, value, metric), y=0, yend=value), color="black") +
  geom_point(aes(x=reorder_within(Player, value, metric), y=value),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, value, metric), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/IntReturnScores.png",
       height = 4,
       width = 12)


# Route Metrics -----------------------------------------------------------

route_ratings = read.csv("~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_route_overall.csv")

route_ratings %>%
  filter(route %in% c('SLANT', 'GO')) %>%
  inner_join(playerTeams %>%
               rename(nflId_def = nflId)) %>%
  inner_join(team_images %>%
               rename(team = teamName)) %>%
    dplyr::select(Player, route, EPS, teamImageUrl, playerImageUrl, displayName) %>%
  rowwise() %>%
  mutate(playerImageUrl = as.character(playerImageUrl)) %>%
  mutate(playerImageUrl = str_split(as.character(playerImageUrl), "&w")[[1]][1]) %>%
  arrange(route, desc(EPS)) %>%
  group_by(route) %>%
  filter(row_number() <= 15) %>%
  mutate(Player = paste0(# as.character(row_number()), 
    # ". ",
    "<img src='",
    playerImageUrl,
    "' width='20' />",
    " ",
    as.character(row_number()),
    ". ", 
    as.character(displayName),
    " <img src='",
    as.character(teamImageUrl),
    "' height='15' />"))%>%
  ungroup() %>%
  arrange(desc(EPS)) %>%
  mutate(Player = as.factor(Player),
         Player = fct_reorder(Player, desc(row_number()))) %>%
  ggplot() +
  geom_segment(aes(x=reorder_within(Player, EPS, route),
                   xend=reorder_within(Player, EPS, route), y=0, yend=EPS), color="black") +
  geom_point(aes(x=reorder_within(Player, EPS, route), y=EPS),
             pch = 21, fill = "white", color = "black") + 
  # geom_image(aes(x = reorder_within(Player, EPS, route), image = playerImageUrl, y = value),  # add geom_image layer
  #           size = 0.1) +
  # scale_x_discrete(name = NULL, labels = teamImageUrl) +
  coord_flip() +
  scale_x_reordered() +
  mytheme +
  # theme_update(axis.text.y = element_text(hjust=0)) +
  facet_wrap(~route, scales = "free_y") +
  labs(y = "Points Saved",
       x = "Player Name (Team)")

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/RoutesScores.png",
       height = 4,
       width = 12)
