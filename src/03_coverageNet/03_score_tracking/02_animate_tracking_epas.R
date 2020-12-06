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

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week3.csv")

epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

pass_attempt_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv")
pass_arrived_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv")
pass_caught_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")

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
      filter(gameId == 2018092309,
            playId == 138) %>%
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
  left_join(pass_attempt_epa_data %>%
              dplyr::select(-ends_with("_prob"))) %>%
  left_join(pass_arrived_epa_data) %>%
  left_join(pass_caught_epa_data %>%
              dplyr::select(-epa))

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE), -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play = 
  ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
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
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(data = example.play, aes(x = (xmax-y), y = x + 10, shape = team,
                                      fill = team, group = nflId, size = team, colour = team), alpha = 0.7) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x + 10, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
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
  labs(title = toString(example.play.info$coverage),
       subtitle = paste0(toString(example.play.info$playDescription),
                  "\n", "EPA = ", toString(round(example.play.info$epa, 2)))) +
  transition_time(frameId)  +
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
       title = "Expected Points Added of Target over Time") +
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

