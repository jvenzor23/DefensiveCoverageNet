# This code animates a given play in the player tracking data

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


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
coverages_week1 = read.csv("~/Desktop/CoverageNet/inputs/coverages_week1.csv")

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week1.csv")


pass_attempt_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv")
pass_arrived_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv")
pass_caught_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")
# Animating a Play --------------------------------------------------------

## 2018090910, 3171
## 2018090902 2481
## dplyr::select( Play
example.play = pbp_data %>%
  inner_join(
    pbp_data %>%
      dplyr::select(gameId, playId) %>%
      # filter(gameId == 2018102200,
      #      playId == 2196) %>%
      # distinct()
      sample_n(1)
  )


example.play.info = plays %>%
  inner_join(example.play %>%
               dplyr::select(gameId, playId) %>%
               distinct()) %>%
  inner_join(targeted_receiver) %>%
  inner_join(coverages_week1) %>%
  left_join(my_epa) %>%
  left_join(pass_attempt_epa_data %>%
              dplyr::select(gameId, playId, epa_pass_attempt)) %>%
  left_join(pass_arrived_epa_data %>%
              dplyr::select(gameId, playId, epa_pass_arrived)) %>%
  left_join(pass_caught_epa_data %>%
              dplyr::select(gameId, playId, epa_throw, epa_yac, epa_yaint)) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                     yardsToGo),
                                   down == 2 ~ paste("2nd and",
                                                     yardsToGo),
                                   down == 3 ~ paste("3rd and",
                                                     yardsToGo),
                                   TRUE ~ paste("4th and",
                                                     yardsToGo)))

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 15, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (example.play %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + example.play.info$yardsToGo

animate.play = 
  ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("polygon", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, yardline + 10, yardline + 10, ymin), colour = "black",
           fill = "limegreen",
           alpha = .5
           ) + 
  annotate("polygon", x = c(xmin, xmin, xmax, xmax), 
           y = c(yardline + 10, firstDownYardLine + 10, firstDownYardLine + 10, yardline + 10), colour = "black",
           fill = "limegreen",
           alpha = .85
  ) + 
  annotate("polygon", x = c(xmin, xmin, xmax, xmax), 
           y = c(firstDownYardLine + 10, ymax, ymax, firstDownYardLine + 10), colour = "black",
           fill = "limegreen",
           alpha = .5
  ) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  geom_segment(aes(x = 0, xend = xmax,
                   y = yardline + 10, yend = yardline + 10),
               color = "blue",
               size = 1,
               alpha = .7) +
  geom_segment(aes(x = 0, xend = xmax,
                   y = firstDownYardLine + 10, yend = firstDownYardLine + 10),
               color = "yellow",
               size = 1) +
  geom_point(data = example.play, aes(x = (xmax-y), y = x + 10, shape = team,
                                      fill = team, group = nflId, size = team, colour = team), alpha = 0.9) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  geom_line(data = example.play %>%
              filter(IsOnOffense,
                     !is.na(nflId),
                     position != "QB"), 
            aes(x = (xmax-y), y = x + 10, group = nflId), 
            alpha = 0.5, size = .5) + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(title = paste0("Down and Distance: ", toString(example.play.info$DownDesc)),
       subtitle = paste0(
                  "\n", "EPA (nflWAR) = ", gsub("-", "\u2013", toString(round(example.play.info$my_epa, 2))),
                  "\n", "EPA Pass Attempt = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_attempt, 2), "N/A"))),
                  "\n", "EPA Pass Arrived = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_arrived, 2), "N/A"))),
                  "\n", "EPA Pass Caught = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_throw, 2), "N/A"))),
                  "\n", "EPA YAC = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_yac, 2), "N/A"))),
                  "\n", "EPA INT Return = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_yaint, 2), "N/A"))))
       ) +
  transition_reveal(frameId)  +
  ease_aes('linear')

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frameId))
animate(animate.play, fps = 10, nframe = play.length.ex)

