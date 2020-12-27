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

epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

pass_attempt_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, IN_prob, epa_pass_attempt) %>%
  rename(c_prob_pass_attempt = C_prob,
         in_prob_pass_attempt = IN_prob)
pass_arrived_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob,IN_prob,epa_pass_arrived) %>%
  rename(c_prob_pass_arrived = C_prob,
         in_prob_pass_arrived = IN_prob)
pass_caught_epa_data = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")
my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

pass_arrived_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)


# Tracking Examples --------------------------------------------------------

# GOOD
pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week13.csv")

pbp_data_clean = pbp_data %>%
  inner_join(pass_arrived_epa_data %>%
               distinct(gameId, playId))

library(magick)
tracking_good_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/tracking_good.gif"),"x400")

# 2018123000/2528 Jarvis TD week 17
# 2018123000/2239 cool pass to Higgins where the play takes a long time
example.play = pbp_data %>%
  inner_join(
    pbp_data %>%
      dplyr::select(gameId, playId) %>%
      filter(gameId == 2018120200,
             playId == 749) %>%
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
  mutate(targeted = if_else(targetNflId == 2560854, 1, 0))

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
  left_join(targeted_receiver) %>%
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


game.info = games %>%
  inner_join(example.play %>%
               dplyr::select(gameId, playId) %>%
               distinct())

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
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) + 
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) + 
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
               filter(nflId == 2560854), aes(x = (xmax-y), y = x + 10),
             size = 6, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
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
  labs(title = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                      " (", game.info$gameDate, ")"),
       subtitle = trimws(paste0("Down and Distance: ", toString(example.play.info$DownDesc), "\n", "\n",
                                paste(strwrap(paste("Play Description:", toString(example.play.info$playDescription))), collapse="\n")))) +
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
         "\n", "EPA Pass Attempt = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_attempt, 2), "N/A"))),
         "\n", "EPA Pass Arrived = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_arrived, 2), "N/A"))),
         "\n", "EPA Pass Caught = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_throw, 2), "N/A"))),
         "\n", "EPA (nflWAR) = ", gsub("-", "\u2013", toString(round(example.play.info$my_epa, 2))),
         "\n")) +
  theme_minimal() +
  transition_time(frameId)  +
  ease_aes('linear')

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.epa_tracking_line_plot$frameId))


b_gif <- animate(animate.epas, fps = 5, nframe = play.length.ex,
                 height = 400, width = 400)
a_gif <- animate(animate.play, fps = 5, nframe = play.length.ex,
                 height = 400, width = 400)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:play.length.ex){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

final_gif = image_append(c(image_crop(tracking_good_nfl_gif[1], "800x400+200"), new_gif[1]))
for(i in 2:play.length.ex){
  if(i <= (play.length.ex - length(tracking_good_nfl_gif)) + 1){
    combined <- image_append(c(image_crop(tracking_good_nfl_gif[1], "800x400+200"), new_gif[i]))
  }else{
    combined <- image_append(c(image_crop(tracking_good_nfl_gif[length(tracking_good_nfl_gif) - (play.length.ex - i)], "800x400+200"), new_gif[i]))
  }
  final_gif <- c(final_gif, combined)
}

final_gif

library(gifski)
anim_save("~/Desktop/CoverageNet/src/08_writeup/images/TrackingGoodEx.gif",
          final_gif,
          fps = 5,
          nframe = play.length.ex,
          height = 500,
          width = 1000,
          res = 120)

tracking_good_nfl_gif[1]
image_crop(tracking_good_nfl_gif[1], "800x400+200")

# BAD
pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week11.csv")

pbp_data_clean = pbp_data %>%
  inner_join(pass_arrived_epa_data %>%
               distinct(gameId, playId))

tracking_bad_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/tracking_bad.gif"),"x400")
tracking_bad_nfl_gif

# 2018123000/2528 Jarvis TD week 17
# 2018123000/2239 cool pass to Higgins where the play takes a long time
example.play = pbp_data %>%
  inner_join(
    pbp_data %>%
      dplyr::select(gameId, playId) %>%
      filter(gameId == 2018111800,
             playId == 2568) %>%
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
  mutate(targeted = if_else(targetNflId == 2535698, 1, 0))

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
  left_join(targeted_receiver) %>%
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


game.info = games %>%
  inner_join(example.play %>%
               dplyr::select(gameId, playId) %>%
               distinct())

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
  scale_fill_manual(values = c("grey", "#654321", "#002244"), guide = FALSE) + 
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
               filter(nflId == 2535698), aes(x = (xmax-y), y = x + 10),
             size = 6, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
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
  labs(title = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                      " (", game.info$gameDate, ")"),
       subtitle = trimws(paste0("Down and Distance: ", toString(example.play.info$DownDesc), "\n", "\n",
                                paste(strwrap(paste("Play Description:", toString(example.play.info$playDescription))), collapse="\n")))) +
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
         "\n", "EPA Pass Attempt = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_attempt, 2), "N/A"))),
         "\n", "EPA Pass Arrived = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_pass_arrived, 2), "N/A"))),
         "\n", "EPA Pass Caught = ", gsub("-", "\u2013", toString(replace_na(round(example.play.info$epa_throw, 2), "N/A"))),
         "\n", "EPA (nflWAR) = ", gsub("-", "\u2013", toString(round(example.play.info$my_epa, 2))),
         "\n")) +
  theme_minimal() +
  transition_time(frameId)  +
  ease_aes('linear')

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.epa_tracking_line_plot$frameId))


b_gif <- animate(animate.epas, fps = 5, nframe = play.length.ex,
                 height = 400, width = 400)
a_gif <- animate(animate.play, fps = 5, nframe = play.length.ex,
                 height = 400, width = 400)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:play.length.ex){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

final_gif = image_append(c(image_crop(tracking_bad_nfl_gif[1], "500x400+100"), new_gif[1]))
for(i in 2:play.length.ex){
  if(i <= (play.length.ex - length(tracking_bad_nfl_gif)) + 1){
    combined <- image_append(c(image_crop(tracking_bad_nfl_gif[1], "500x400+100"), new_gif[i]))
  }else{
    combined <- image_append(c(image_crop(tracking_bad_nfl_gif[length(tracking_bad_nfl_gif) - (play.length.ex - i)], "500x400+100"), new_gif[i]))
  }
  final_gif <- c(final_gif, combined)
}

final_gif

image_crop(tracking_bad_nfl_gif[1], "500x400+100")

library(gifski)
anim_save("~/Desktop/CoverageNet/src/08_writeup/images/TrackingBadEx.gif",
          final_gif,
          fps = 5,
          nframe = play.length.ex,
          height = 500,
          width = 1000,
          res = 120)

tracking_good_nfl_gif[1]
image_crop(tracking_good_nfl_gif[1], "800x400+200")

# Closing Examples --------------------------------------------------------

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week13.csv")

play3 = pbp_data %>%
  filter(gameId == 2018120205,
         playId == 1415)

play3_desc = plays %>%
  inner_join(play3 %>%
               distinct(gameId, playId)) %>%
  dplyr::select(-epa) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                yardsToGo),
                              down == 2 ~ paste("2nd and",
                                                yardsToGo),
                              down == 3 ~ paste("3rd and",
                                                yardsToGo),
                              TRUE ~ paste("4th and",
                                           yardsToGo))) %>%
  inner_join(pass_attempt_epa_data) %>%
  inner_join(pass_arrived_epa_data) %>%
  inner_join(pass_caught_epa_data)


play3 %>% distinct(event)

play3_clipped = rbind(play3 %>%
                        filter(event %in% c('pass_forward',
                                            'pass_outcome_interception',
                                            'out_of_bounds')),
                      play3 %>%
                        inner_join(pass_arrived_frames) %>%
                        mutate(event = "pass_arrived")) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(event = as.factor(event)) %>%
  mutate(event = factor(event, levels = c("pass_forward",
                                          "pass_arrived",
                                          "pass_outcome_interception",
                                          "out_of_bounds")))

game.info = games %>%
  inner_join(play3_clipped %>%
               dplyr::select(gameId, playId) %>%
               distinct())

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(play3_clipped$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(play3_clipped$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (play3_clipped %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + play3_desc$yardsToGo

pass_forward3 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("grey", "#654321", "#002244"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play3_clipped %>%
                 filter(event == "pass_forward") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_forward"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play3_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_forward"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play3_clipped %>%
              filter(event == "pass_forward") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_forward") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play3_desc$epa_pass_attempt, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$c_prob_pass_attempt, 4), .1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$in_prob_pass_attempt, 4), .1), "N/A")))
       ))

pass_arrived3 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("grey", "#654321", "#002244"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play3_clipped %>%
                 filter(event == "pass_arrived") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play3_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play3_clipped %>%
              filter(event == "pass_arrived") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_arrived") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play3_desc$epa_pass_arrived, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$c_prob_pass_arrived, 4), accuracy = 0.1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$in_prob_pass_arrived, 4), accuracy = 0.1), "N/A")))
       ))


gridExtra::grid.arrange(pass_forward3, 
                        pass_arrived3,
                        ncol = 2,
                        top = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                     " (", game.info$gameDate, ")",
                                     "\n","Down and Distance: ", toString(play3_desc$DownDesc), "\n",
                                     paste(strwrap(paste("Play Description:", toString(play3_desc$playDescription))), collapse="\n")))
library(grid)
g3 <- gridExtra::arrangeGrob(pass_forward3, 
                             pass_arrived3,
                             ncol = 2,
                             top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                          " (", game.info$gameDate, ")",
                                          "\n","Down and Distance: ", toString(play3_desc$DownDesc), "\n",
                                          paste(strwrap(paste("Play Description:", toString(play3_desc$playDescription))), collapse="\n")),
                                          gp=gpar(fontsize=8,font=8)))
        
ggsave(plot = g3, 
       filename = "~/Desktop/CoverageNet/src/08_writeup/intermediates/closing_good_intermediate.png",
       height = 4,
       width = 6)

closing_good_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/closing_good.gif"),"x400")
closing_good_nfl_gif
closing_good_image = image_scale(image_read("~/Desktop/CoverageNet/src/08_writeup/intermediates/closing_good_intermediate.png"),"x400")
closing_good_image

final_gif = image_append(c(image_crop(closing_good_nfl_gif[1], "450x400"), closing_good_image))
for(i in 2:length(closing_good_nfl_gif)){
    combined <- image_append(c(image_crop(closing_good_nfl_gif[i], "450x400"), closing_good_image))
    final_gif <- c(final_gif, combined)
}

final_gif

library(gifski)
anim_save("~/Desktop/CoverageNet/src/08_writeup/images/ClosingGoodEx.gif",
          final_gif,
          fps = 10,
          nframe = length(closing_good_nfl_gif),
          height = 500,
          width = 1000,
          res = 120)

# closing bad
pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week2.csv")

play3 = pbp_data %>%
  filter(gameId == 2018091602,
         playId == 141)

play3_desc = plays %>%
  inner_join(play3 %>%
               distinct(gameId, playId)) %>%
  dplyr::select(-epa) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                yardsToGo),
                              down == 2 ~ paste("2nd and",
                                                yardsToGo),
                              down == 3 ~ paste("3rd and",
                                                yardsToGo),
                              TRUE ~ paste("4th and",
                                           yardsToGo))) %>%
  inner_join(pass_attempt_epa_data) %>%
  inner_join(pass_arrived_epa_data) %>%
  inner_join(pass_caught_epa_data)


play3 %>% distinct(event)

play3_clipped = rbind(play3 %>%
                        filter(event %in% c('pass_forward',
                                            'pass_outcome_interception',
                                            'out_of_bounds')),
                      play3 %>%
                        inner_join(pass_arrived_frames) %>%
                        mutate(event = "pass_arrived")) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(event = as.factor(event)) %>%
  mutate(event = factor(event, levels = c("pass_forward",
                                          "pass_arrived",
                                          "pass_outcome_interception",
                                          "out_of_bounds")))

game.info = games %>%
  inner_join(play3_clipped %>%
               dplyr::select(gameId, playId) %>%
               distinct())

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(play3_clipped$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(play3_clipped$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (play3_clipped %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + play3_desc$yardsToGo

pass_forward3 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play3_clipped %>%
                 filter(event == "pass_forward") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_forward"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play3_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_forward"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play3_clipped %>%
              filter(event == "pass_forward") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_forward") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play3_desc$epa_pass_attempt, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$c_prob_pass_attempt, 4), .1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$in_prob_pass_attempt, 4), .1), "N/A")))
       ))

pass_arrived3 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play3_clipped %>%
                 filter(event == "pass_arrived") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play3_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play3_clipped %>%
              filter(event == "pass_arrived") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play3_clipped %>%
               filter(event == "pass_arrived") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play3_desc$epa_pass_arrived, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$c_prob_pass_arrived, 4), accuracy = 0.1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play3_desc$in_prob_pass_arrived, 4), accuracy = 0.1), "N/A")))
       ))


gridExtra::grid.arrange(pass_forward3, 
                        pass_arrived3,
                        ncol = 2,
                        top = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                     " (", game.info$gameDate, ")",
                                     "\n","Down and Distance: ", toString(play3_desc$DownDesc), "\n",
                                     paste(strwrap(paste("Play Description:", toString(play3_desc$playDescription))), collapse="\n")))
library(grid)
g3 <- gridExtra::arrangeGrob(pass_forward3, 
                             pass_arrived3,
                             ncol = 2,
                             top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                                   " (", game.info$gameDate, ")",
                                                   "\n","Down and Distance: ", toString(play3_desc$DownDesc), "\n",
                                                   paste(strwrap(paste("Play Description:", toString(play3_desc$playDescription))), collapse="\n")),
                                            gp=gpar(fontsize=8,font=8)))

ggsave(plot = g3, 
       filename = "~/Desktop/CoverageNet/src/08_writeup/intermediates/closing_bad_intermediate.png",
       height = 4,
       width = 6)

closing_bad_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/closing_bad.gif"),"x400")
closing_bad_nfl_gif
closing_bad_image = image_scale(image_read("~/Desktop/CoverageNet/src/08_writeup/intermediates/closing_bad_intermediate.png"),"x400")
closing_bad_image

final_gif = image_append(c(image_crop(closing_bad_nfl_gif[1], "450x400"), closing_bad_image))
for(i in 2:length(closing_bad_nfl_gif)){
  combined <- image_append(c(image_crop(closing_bad_nfl_gif[i], "450x400"), closing_bad_image))
  final_gif <- c(final_gif, combined)
}

final_gif

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/ClosingBadEx.gif",
          final_gif,
          fps = 10,
          nframe = length(closing_bad_nfl_gif),
          height = 500,
          width = 1000,
          res = 120)

# Ball Skills Examples ----------------------------------------------------

# GOOD
pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week5.csv")

play1 = pbp_data %>%
  filter(gameId == 2018100702,
         playId == 3173)

play1_desc = plays %>%
  inner_join(play1 %>%
               distinct(gameId, playId)) %>%
  dplyr::select(-epa) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                yardsToGo),
                              down == 2 ~ paste("2nd and",
                                                yardsToGo),
                              down == 3 ~ paste("3rd and",
                                                yardsToGo),
                              TRUE ~ paste("4th and",
                                           yardsToGo))) %>%
  inner_join(pass_attempt_epa_data) %>%
  inner_join(pass_arrived_epa_data) %>%
  inner_join(pass_caught_epa_data)


play1 %>% distinct(event)

play1_clipped = rbind(play1 %>%
                        filter(event %in% c('pass_forward',
                                            'pass_outcome_incomplete',
                                            'tackle')),
                      play1 %>%
                        inner_join(pass_arrived_frames) %>%
                        mutate(event = "pass_arrived")) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  mutate(x_end = .75*s*cos((90-dir)*pi/180) + x, 
         y_end = .75*s*sin((90-dir)*pi/180) + y) %>%
  mutate(event = as.factor(event)) %>%
  mutate(event = factor(event, levels = c("pass_forward",
                                          "pass_arrived",
                                          "pass_outcome_incomplete",
                                          "tackle")))

game.info = games %>%
  inner_join(play1_clipped %>%
               dplyr::select(gameId, playId) %>%
               distinct())

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(play1_clipped$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(play1_clipped$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (play1_clipped %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + play1_desc$yardsToGo

pass_arrived1 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play1_clipped %>%
                 filter(event == "pass_arrived") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play1_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play1_clipped %>%
              filter(event == "pass_arrived") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_arrived") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play1_desc$epa_pass_arrived, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play1_desc$c_prob_pass_arrived, 4), accuracy = 0.1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play1_desc$in_prob_pass_arrived, 4), accuracy = 0.1), "N/A")))
       ))

pass_caught1 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play1_clipped %>%
                 filter(event == "pass_outcome_incomplete") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_outcome_incomplete"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                           fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play1_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_outcome_incomplete"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play1_clipped %>%
              filter(event == "pass_outcome_incomplete") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_outcome_incomplete") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Result = ", gsub("-", "\u2013", toString(replace_na(round(play1_desc$epa_throw, 3), "N/A")))),
       subtitle = "")

gridExtra::grid.arrange(pass_arrived1,
                        pass_caught1,
                        ncol = 2,
                        top = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                     " (", game.info$gameDate, ")",
                                     "\n","Down and Distance: ", toString(play1_desc$DownDesc), "\n",
                                     paste(strwrap(paste("Play Description:", toString(play1_desc$playDescription))), collapse="\n")))
library(grid)
g1 <- gridExtra::arrangeGrob(pass_arrived1, 
                             pass_caught1,
                             ncol = 2,
                             top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                                   " (", game.info$gameDate, ")",
                                                   "\n","Down and Distance: ", toString(play1_desc$DownDesc), "\n",
                                                   paste(strwrap(paste("Play Description:", toString(play1_desc$playDescription))), collapse="\n")),
                                            gp=gpar(fontsize=8,font=8)))

ggsave(plot = g1, 
       filename = "~/Desktop/CoverageNet/src/08_writeup/intermediates/ball_skills_good_intermediate.png",
       height = 4,
       width = 6)

ball_skills_good_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/ball_skills_good.gif"),"x400")
ball_skills_good_nfl_gif
ball_skills_good_image = image_scale(image_read("~/Desktop/CoverageNet/src/08_writeup/intermediates/ball_skills_good_intermediate.png"),"x400")
ball_skills_good_image

final_gif = image_append(c(image_crop(ball_skills_good_nfl_gif[1], "450x400"), ball_skills_good_image))
for(i in 2:length(ball_skills_good_nfl_gif)){
  combined <- image_append(c(image_crop(ball_skills_good_nfl_gif[i], "450x400"), ball_skills_good_image))
  final_gif <- c(final_gif, combined)
}

final_gif

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/BallSkillsGoodEx.gif",
          final_gif,
          fps = 10,
          nframe = length(ball_skills_good_nfl_gif),
          height = 500,
          width = 1000,
          res = 120)

# BAD
pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week10.csv")

play1 = pbp_data %>%
  filter(gameId == 2018110800,
         playId == 1602)

play1_desc = plays %>%
  inner_join(play1 %>%
               distinct(gameId, playId)) %>%
  dplyr::select(-epa) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                yardsToGo),
                              down == 2 ~ paste("2nd and",
                                                yardsToGo),
                              down == 3 ~ paste("3rd and",
                                                yardsToGo),
                              TRUE ~ paste("4th and",
                                           yardsToGo))) %>%
  inner_join(pass_attempt_epa_data) %>%
  inner_join(pass_arrived_epa_data) %>%
  inner_join(pass_caught_epa_data)


play1 %>% distinct(event)

play1_clipped = rbind(play1 %>%
                        filter(event %in% c('pass_forward',
                                            'pass_outcome_caught',
                                            'tackle')),
                      play1 %>%
                        inner_join(pass_arrived_frames) %>%
                        mutate(event = "pass_arrived")) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  mutate(x_end = .75*s*cos((90-dir)*pi/180) + x, 
         y_end = .75*s*sin((90-dir)*pi/180) + y) %>%
  mutate(event = as.factor(event)) %>%
  mutate(event = factor(event, levels = c("pass_forward",
                                          "pass_arrived",
                                          "pass_outcome_caught",
                                          "tackle")))

game.info = games %>%
  inner_join(play1_clipped %>%
               dplyr::select(gameId, playId) %>%
               distinct())

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(play1_clipped$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(play1_clipped$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (play1_clipped %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + play1_desc$yardsToGo

pass_arrived1 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play1_clipped %>%
                 filter(event == "pass_arrived") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play1_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_arrived"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play1_clipped %>%
              filter(event == "pass_arrived") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_arrived") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play1_desc$epa_pass_arrived, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play1_desc$c_prob_pass_arrived, 4), accuracy = 0.1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play1_desc$in_prob_pass_arrived, 4), accuracy = 0.1), "N/A")))
       ))

pass_caught1 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play1_clipped %>%
                 filter(event == "pass_outcome_caught") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_outcome_caught"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                               fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play1_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_outcome_caught"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play1_clipped %>%
              filter(event == "pass_outcome_caught") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play1_clipped %>%
               filter(event == "pass_outcome_caught") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play1_desc$epa_throw, 3), "N/A")))),
       subtitle = paste0("Expected YAC = ", gsub("-", "\u2013", toString(replace_na(round(play1_desc$eyac, 1), "N/A"))))
  )

gridExtra::grid.arrange(pass_arrived1,
                        pass_caught1,
                        ncol = 2,
                        top = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                     " (", game.info$gameDate, ")",
                                     "\n","Down and Distance: ", toString(play1_desc$DownDesc), "\n",
                                     paste(strwrap(paste("Play Description:", toString(play1_desc$playDescription))), collapse="\n")))
library(grid)
g1 <- gridExtra::arrangeGrob(pass_arrived1, 
                             pass_caught1,
                             ncol = 2,
                             top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                                   " (", game.info$gameDate, ")",
                                                   "\n","Down and Distance: ", toString(play1_desc$DownDesc), "\n",
                                                   paste(strwrap(paste("Play Description:", toString(play1_desc$playDescription))), collapse="\n")),
                                            gp=gpar(fontsize=8,font=8)))

ggsave(plot = g1, 
       filename = "~/Desktop/CoverageNet/src/08_writeup/intermediates/ball_skills_bad_intermediate.png",
       height = 4,
       width = 6)

ball_skills_bad_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/ball_skills_bad.gif"),"x400")
ball_skills_bad_nfl_gif
ball_skills_bad_image = image_scale(image_read("~/Desktop/CoverageNet/src/08_writeup/intermediates/ball_skills_bad_intermediate.png"),"x400")
ball_skills_bad_image

final_gif = image_append(c(image_crop(ball_skills_bad_nfl_gif[1], "450x400"), ball_skills_bad_image))
for(i in 2:length(ball_skills_bad_nfl_gif)){
  combined <- image_append(c(image_crop(ball_skills_bad_nfl_gif[i], "450x400"), ball_skills_bad_image))
  final_gif <- c(final_gif, combined)
}

final_gif

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/BallSkillsBadEx.gif",
          final_gif,
          fps = 10,
          nframe = length(ball_skills_bad_nfl_gif),
          height = 500,
          width = 1000,
          res = 120)

# Tackling Examples -------------------------------------------------------

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week10.csv")

play2 = pbp_data %>%
  filter(gameId == 2018111104,
         playId == 481)

play2_desc = plays %>%
  inner_join(play2 %>%
               distinct(gameId, playId)) %>%
  dplyr::select(-epa) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                yardsToGo),
                              down == 2 ~ paste("2nd and",
                                                yardsToGo),
                              down == 3 ~ paste("3rd and",
                                                yardsToGo),
                              TRUE ~ paste("4th and",
                                           yardsToGo))) %>%
  inner_join(pass_attempt_epa_data) %>%
  inner_join(pass_arrived_epa_data) %>%
  inner_join(pass_caught_epa_data)


play2 %>% distinct(event)

play2_clipped = rbind(play2 %>%
                        filter(event %in% c('pass_forward',
                                            'pass_outcome_caught',
                                            'tackle')),
                      play2 %>%
                        inner_join(pass_arrived_frames) %>%
                        mutate(event = "pass_arrived")) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(event = as.factor(event)) %>%
  mutate(event = factor(event, levels = c("pass_forward",
                                          "pass_arrived",
                                          "pass_outcome_caught",
                                          "tackle")))

game.info = games %>%
  inner_join(play2_clipped %>%
               dplyr::select(gameId, playId) %>%
               distinct())

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(play2_clipped$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(play2_clipped$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (play2_clipped %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + play2_desc$yardsToGo

pass_caught2 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play2_clipped %>%
                 filter(event == "pass_outcome_caught") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play2_clipped %>%
               filter(event == "pass_outcome_caught"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                           fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play2_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_outcome_caught"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play2_clipped %>%
              filter(event == "pass_outcome_caught") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play2_clipped %>%
               filter(event == "pass_outcome_caught") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play2_desc$epa_throw, 3), "N/A")))),
       subtitle = paste0("Expected YAC = ", gsub("-", "\u2013", toString(replace_na(round(play2_desc$eyac, 3), "N/A"))))
  )

pass_tackle2 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#002244", "#654321", "grey"), guide = FALSE) +
  scale_colour_manual(values = c("#c60c30", "#654321", "black"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  # geom_segment(data = play2_clipped %>%
  #                filter(event == "tackle") %>%
  #                filter(!is.na(nflId)),
  #              aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
  #                  yend = x_end + 10, group = nflId),
  #              color = "black",
  #              arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play2_clipped %>%
               filter(event == "tackle"), aes(x = (xmax-y), y = x + 10, shape = team,
                                              fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play2_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "tackle"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play2_clipped %>%
              filter(event == "tackle") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play2_clipped %>%
               filter(event == "tackle") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA (nflWAR) = ", gsub("-", "\u2013", toString(replace_na(round(play2_desc$epa, 3), "N/A")))),
       subtitle = ""
  )

gridExtra::grid.arrange(pass_caught2, 
                        pass_tackle2,
                        ncol = 2,
                        top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                              " (", game.info$gameDate, ")",
                                              "\n","Down and Distance: ", toString(play2_desc$DownDesc), "\n",
                                              paste(strwrap(paste("Play Description:", toString(play2_desc$playDescription))), collapse="\n")),
                                       gp=gpar(fontsize=8,font=8)))
library(grid)
g1 <- gridExtra::arrangeGrob(pass_caught2, 
                             pass_tackle2,
                             ncol = 2,
                             top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                                   " (", game.info$gameDate, ")",
                                                   "\n","Down and Distance: ", toString(play2_desc$DownDesc), "\n",
                                                   paste(strwrap(paste("Play Description:", toString(play2_desc$playDescription))), collapse="\n")),
                                            gp=gpar(fontsize=8,font=8)))

ggsave(plot = g1, 
       filename = "~/Desktop/CoverageNet/src/08_writeup/intermediates/tackle_good_intermediate.png",
       height = 4,
       width = 6)

tackle_good_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/tackling_good.gif"),"x400")
tackle_good_nfl_gif
tackle_good_image = image_scale(image_read("~/Desktop/CoverageNet/src/08_writeup/intermediates/tackle_good_intermediate.png"),"x400")
tackle_good_image

final_gif = image_append(c(image_crop(tackle_good_nfl_gif[1], "450x400"), tackle_good_image))
for(i in 2:length(tackle_good_nfl_gif)){
  combined <- image_append(c(image_crop(tackle_good_nfl_gif[i], "450x400"), tackle_good_image))
  final_gif <- c(final_gif, combined)
}

final_gif

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/TacklingGoodEx.gif",
          final_gif,
          fps = 10,
          nframe = length(tackle_good_nfl_gif),
          height = 500,
          width = 1000,
          res = 120)



# Ball Hawk Examples ------------------------------------------------------

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week1.csv")

play2 = pbp_data %>%
  filter(gameId == 2018090901,
         playId == 704)

play2_desc = plays %>%
  inner_join(play2 %>%
               distinct(gameId, playId)) %>%
  dplyr::select(-epa) %>%
  mutate(DownDesc = case_when(down == 1 ~ paste("1st and",
                                                yardsToGo),
                              down == 2 ~ paste("2nd and",
                                                yardsToGo),
                              down == 3 ~ paste("3rd and",
                                                yardsToGo),
                              TRUE ~ paste("4th and",
                                           yardsToGo))) %>%
  left_join(pass_attempt_epa_data) %>%
  left_join(pass_arrived_epa_data) %>%
  left_join(pass_caught_epa_data)


play2 %>% distinct(event)

play2_clipped = rbind(play2 %>%
                        filter(event %in% c('pass_forward',
                                            'pass_outcome_interception',
                                            'tackle')),
                      play2 %>%
                        inner_join(pass_arrived_frames) %>%
                        mutate(event = "pass_arrived")) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(event = as.factor(event)) %>%
  mutate(event = factor(event, levels = c("pass_forward",
                                          "pass_arrived",
                                          "pass_outcome_interception",
                                          "tackle")))

game.info = games %>%
  inner_join(play2_clipped %>%
               dplyr::select(gameId, playId) %>%
               distinct())

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

## Specific boundaries for a given play
ymin <- max(round(min(play2_clipped$x, na.rm = TRUE), -1), 0) + 5
ymax <- min(round(max(play2_clipped$x, na.rm = TRUE) + 20, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

yardline = (play2_clipped %>% distinct(YardsFromOwnGoal))$YardsFromOwnGoal
firstDownYardLine = yardline + play2_desc$yardsToGo

pass_forward2 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("grey", "#654321", "#002244"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play2_clipped %>%
                 filter(event == "pass_forward") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play2_clipped %>%
               filter(event == "pass_forward"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                    fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play2_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_forward"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play2_clipped %>%
              filter(event == "pass_forward") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play2_clipped %>%
               filter(event == "pass_forward") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play2_desc$epa_pass_attempt, 3), "N/A")))),
       subtitle = paste0("C% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play2_desc$c_prob_pass_attempt, 4), .1), "N/A"))),
                         ", INT% = ", gsub("-", "\u2013", toString(replace_na(scales::percent(round(play2_desc$in_prob_pass_attempt, 4), .1), "N/A")))
       ))

pass_caught2 = 
  ggplot() +
  scale_size_manual(values = c(4, 2.5, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("grey", "#654321", "#002244"), guide = FALSE) +
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
  annotate("segment", x = 0, xend = xmax,
           y = yardline + 10, yend = yardline + 10,
           color = "blue",
           size = 1,
           alpha = .7) +
  annotate("segment", x = 0, xend = xmax,
           y = firstDownYardLine + 10, yend = firstDownYardLine + 10,
           color = "yellow",
           size = 1,
           alpha = .7) +
  geom_segment(data = play2_clipped %>%
                 filter(event == "pass_outcome_interception") %>%
                 filter(!is.na(nflId)),
               aes(x = (xmax - y), y = x + 10, xend = (xmax - y_end),
                   yend = x_end + 10, group = nflId),
               color = "black",
               arrow = arrow(length = unit(.25,"cm"))) +
  geom_point(data = play2_clipped %>%
               filter(event == "pass_outcome_interception"), aes(x = (xmax-y), y = x + 10, shape = team,
                                                           fill = team, group = nflId, size = team, colour = team), alpha = 0.9) +
  geom_point(data = play2_clipped %>%
               inner_join(targeted_receiver %>% rename(nflId = targetNflId)) %>%
               filter(event == "pass_outcome_interception"), aes(x = (xmax-y), y = x + 10),
             size = 4, color = "black", shape = 21, fill = "#e31837", alpha = 0.9) +
  geom_text(data = play2_clipped %>%
              filter(event == "pass_outcome_interception") %>%
              filter(!is.na(nflId)), aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white",
            vjust = 0.36, size = 2) +
  geom_point(data = play2_clipped %>%
               filter(event == "pass_outcome_interception") %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10),
             fill = "brown", color = "#654321", shape = 16, size = 2.5,
             alpha = 1) +
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
        plot.background=element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8)) +
  facet_wrap(~event) +
  labs(title = paste0("EPA Prediction = ", gsub("-", "\u2013", toString(replace_na(round(play2_desc$epa_throw, 3), "N/A")))),
       subtitle = paste0("Expected YAINT = ", gsub("-", "\u2013", toString(replace_na(round(play2_desc$eyaint, 3), "N/A"))))
  )

gridExtra::grid.arrange(pass_forward2,
                        pass_caught2,
                        ncol = 2,
                        top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                              " (", game.info$gameDate, ")",
                                              "\n","Down and Distance: ", toString(play2_desc$DownDesc), "\n",
                                              paste(strwrap(paste("Play Description:", toString(play2_desc$playDescription))), collapse="\n")),
                                       gp=gpar(fontsize=8,font=8)))
library(grid)
g1 <- gridExtra::arrangeGrob(pass_forward2, 
                             pass_caught2,
                             ncol = 2,
                             top = textGrob(paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                                                   " (", game.info$gameDate, ")",
                                                   "\n","Down and Distance: ", toString(play2_desc$DownDesc), "\n",
                                                   paste(strwrap(paste("Play Description:", toString(play2_desc$playDescription))), collapse="\n")),
                                            gp=gpar(fontsize=8,font=8)))

ggsave(plot = g1, 
       filename = "~/Desktop/CoverageNet/src/08_writeup/intermediates/ball_hawk_intermediate.png",
       height = 4,
       width = 6)

ball_hawk_nfl_gif = image_scale(image_read(path = "~/Desktop/CoverageNet/src/08_writeup/NFL_videos/ball_hawk.gif"),"x400")
ball_hawk_nfl_gif
ball_hawk_image = image_scale(image_read("~/Desktop/CoverageNet/src/08_writeup/intermediates/ball_hawk_intermediate.png"),"x400")
ball_hawk_image

final_gif = image_append(c(ball_hawk_nfl_gif[1], ball_hawk_image))
for(i in 2:length(ball_hawk_nfl_gif)){
  combined <- image_append(c(ball_hawk_nfl_gif[i], ball_hawk_image))
  final_gif <- c(final_gif, combined)
}

final_gif

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/BallHawkEx.gif",
          final_gif,
          fps = 10,
          nframe = length(ball_hawk_nfl_gif),
          height = 500,
          width = 1000,
          res = 120)

