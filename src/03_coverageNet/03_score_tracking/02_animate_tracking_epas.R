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

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week17.csv")

epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

pass_attempt_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv")
pass_arrived_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv")
pass_caught_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")
my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

pbp_data = pbp_data %>%
  inner_join(epa_tracking_total %>%
               distinct(gameId, playId))

pbp_data = pbp_data %>%
  inner_join(games %>%
               filter(visitorTeamAbbr == "CLE"))

plays = plays %>%
  inner_join(games %>%
               filter(visitorTeamAbbr == "CLE")) %>%
  inner_join(pbp_data %>%
               distinct(gameId, playId)) %>%
  arrange(playId)

epa_tracking_total %>%
  filter(gameId == 2018123000)
# Animating a Play --------------------------------------------------------

## Select Play

# 2018123000/2528 Jarvis TD week 17
# 2018123000/2239 cool pass to Higgins where the play takes a long time
example.play = pbp_data %>%
  inner_join(
    pbp_data %>%
      dplyr::select(gameId, playId) %>%
      filter(gameId == 2018123000,
            playId == 2528) %>%
      distinct()
      # sample_n(1)
  )

example.epa_tracking_point_plot = epa_tracking_total %>%
  inner_join(example.play %>%
               rename(targetNflId = nflId)) %>%
  mutate(time_after_snap = (frameId - 11)*.1)

example.epa_tracking_point_plot_min_vals = example.epa_tracking_point_plot %>%
  group_by(gameId, playId, targetNflId) %>%
  filter(frameId == min(frameId)) %>%
  inner_join(example.play %>%
               distinct(gameId, playId, frameId) %>%
               rename(frameId2 = frameId)) %>%
  filter(frameId2 < frameId) %>%
  dplyr::select(-frameId) %>%
  rename(frameId = frameId2) %>%
  dplyr::select(names(example.epa_tracking_point_plot))

example.epa_tracking_point_plot_max_vals = example.epa_tracking_point_plot %>%
  group_by(gameId, playId, targetNflId) %>%
  filter(frameId == max(frameId)) %>%
  inner_join(example.play %>%
               distinct(gameId, playId, frameId) %>%
               rename(frameId2 = frameId)) %>%
  filter(frameId2 > frameId) %>%
  dplyr::select(-frameId) %>%
  rename(frameId = frameId2) %>%
  dplyr::select(names(example.epa_tracking_point_plot)) 

example.epa_tracking_point_plot = rbind.data.frame(example.epa_tracking_point_plot,
                                        example.epa_tracking_point_plot_min_vals,
                                        example.epa_tracking_point_plot_max_vals) %>%
  arrange(gameId, playId, frameId, targetNflId) %>%
  left_join(targeted_receiver %>%
         mutate(targeted = 1))

example.epa_tracking_point_plot$targeted[is.na(example.epa_tracking_point_plot$targeted)] = 0

example.epa_tracking_point_plot = example.epa_tracking_point_plot %>%
  mutate(targeted = as.factor(targeted))

example.epa_tracking_line_plot = example.epa_tracking_point_plot %>%
  rename(frameId_new = frameId) %>%
  full_join(example.epa_tracking_point_plot %>%
              dplyr::select(gameId, playId, targetNflId, frameId)) %>%
  filter(frameId_new <= frameId) %>%
  dplyr::select(gameId, playId, frameId, targetNflId, time_after_snap, everything()) %>%
  arrange(gameId, playId, frameId, targetNflId) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(epa_pass_attempt = epa_pass_attempt + rnorm(1)/10000)

example.play.info = plays %>%
  inner_join(example.play %>%
               dplyr::select(gameId, playId) %>%
               distinct()) %>%
  inner_join(targeted_receiver) %>%
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
  geom_point(data = example.play %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10), 
             fill = "brown", color = "#654321", shape = 16, size = 4,
             alpha = 1) + 
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
       subtitle = paste(strwrap(toString(example.play.info$playDescription)), collapse="\n")
  ) +
  transition_reveal(frameId)  +
  ease_aes('linear')

animate.epas = 
  ggplot() +
  scale_size_manual(values = c(6, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 21), guide = FALSE) +
  scale_fill_manual(values = c("grey", "red"), guide = FALSE) + 
  scale_colour_manual(values = c("grey", "red"), guide = FALSE) + 
  scale_alpha_manual(values = c(.5, 1), guide = FALSE) + 
  geom_line(data = example.epa_tracking_line_plot %>% dplyr::select(-frameId_new), aes(x = time_after_snap, y =  epa_pass_attempt,
                                                                                group = targetNflId, colour = targeted, alpha = targeted)) + 
  geom_point(data = example.epa_tracking_point_plot, aes(x = time_after_snap, y =  epa_pass_attempt,
                                      fill = targeted, group = targetNflId, size = targeted, colour = targeted)) + 
  geom_text(data = example.epa_tracking_point_plot, aes(x = time_after_snap, y = epa_pass_attempt, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  geom_hline(yintercept=0, color = "black") +
  ylim(min(example.epa_tracking_point_plot$epa_pass_attempt) - .25, 
       max(example.epa_tracking_point_plot$epa_pass_attempt) + .25) +
  labs(x = "Time after Snap (s)",
       y = "Expected Points Added of Pass Attempt",
       title = "Expected Points Added of Target over Time",
       subtitle = paste0(
         "\n", "EPA (nflWAR) = ", gsub("-", "\u2013", toString(round(example.play.info$my_epa, 2))),
         "\n", "EPA Pass Attempt = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_attempt, 2), "N/A"))),
         "\n", "EPA Pass Arrived = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_arrived, 2), "N/A"))),
         "\n", "EPA Pass Caught = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_throw, 2), "N/A"))),
         "\n", "EPA YAC = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_yac, 2), "N/A"))),
         "\n", "EPA INT Return = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_yaint, 2), "N/A"))))) +
  transition_time(frameId)  +
  ease_aes('linear')

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.epa_tracking_line_plot$frameId))


b_gif <- animate(animate.epas, fps = 5, nframe = play.length.ex,
                 height = 500, width = 500)
a_gif <- animate(animate.play, fps = 5, nframe = play.length.ex,
                 height = 500, width = 500)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:play.length.ex){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

