# This code animates a given play in the player tracking data, and displays
# coverages.
#
# CONVENTION:
# 1) man-coverage: line connecting receiver and defender
# 2) zone-coverage: faded green circle around zone defender
# 3) **LBs are excluded, since the prompt asks for an analysis of
#    defensive backs. Defenders without (1) or (2) are either LBs or DLs

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
library(broom)


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
coverages_week1 = read.csv("~/Desktop/CoverageNet/inputs/coverages_week1.csv")

pt_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week1.csv")

man_coverage = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
closest_pairs = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/defense_off_closest_players.csv")

# Quantifying the Divider -------------------------------------------------

setwd("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/")
files = dir()[startsWith(dir(), "week")]

divider_total = data.frame()
divider_all_total = data.frame()

for(file in files){
  
  
  pt_data = read.csv(paste0("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/",
                            file))

  # identifying plays beginning within the 25 yard line
  within_the_25 = pt_data %>%
    filter(event == "ball_snap",
           is.na(nflId),
           x >= 75) %>%
    distinct(gameId, playId)
  
  # sideline distance from snap
  snap_data = pt_data %>%
    filter(event == "ball_snap",
           is.na(nflId)) %>%
    dplyr::select(gameId, playId, x, y) %>%
    rename(x_snap = x,
           y_snap = y) %>%
    filter(y_snap > 20,
           x_snap < 31)
  
  receiver_snap_data = pt_data %>%
    filter(event == "ball_snap",
           !is.na(nflId),
           IsOnOffense) %>%
    dplyr::select(gameId, playId, nflId, x, y) %>%
    rename(x_snap_receiver = x,
           y_snap_receiver = y,
           nflId_snap_receiver = nflId)
  
  # filtering to between ball snap and pass attempt (or 2.5 seconds!)
  divider = pt_data %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId, nflId) %>%
    filter(cumsum(event == "ball_snap") >= 1) %>%
    group_by(gameId, playId, nflId) %>%
    filter(cumsum((event != "None")&(event != "ball_snap")) < 1) %>%
    group_by(gameId, playId, nflId) %>%
    filter(max(frameId) - min(frameId) <= 30) %>%
    inner_join(man_coverage, 
               by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
    inner_join(pt_data %>%
                 dplyr::select(gameId, playId, frameId, nflId, x, y) %>%
                 rename(x_off = x,
                        y_off = y),
                 by = c("gameId", "playId", "frameId", "nflId_off" = "nflId")) %>%
    anti_join(within_the_25) %>%
    inner_join(closest_pairs,
               by = c("gameId", "playId", "nflId", "nflId_off" = "nflId_opp")) %>%
    filter(frameId >= frameId_start,
           frameId <= frameId_end) %>%
    inner_join(snap_data) %>%
    filter(x <= x_snap + 15,
           x >= x_snap - 2,
           x_off <= x_snap + 15,
           x_off >= x_snap - 2) %>%
    mutate(separation = sqrt((x_off - x)^2 + (y_off - y)^2)) %>%
    mutate(sideline_distance_from_snap = if_else(y < 160/6,
                                                 y_snap,
                                                 160/3 - y_snap)) %>%
    ungroup() %>%
    mutate(outside_leverage = if_else(y < 160/6, 
                                      y_off - y,
                                      y - y_off),
           distance_from_sideline = if_else(y < 160/6, 
                                            y,
                                            160/3 - y)) %>%
    dplyr::select(gameId, playId, frameId, nflId, nflId_off, separation, x,x_off,
                  sideline_distance_from_snap, distance_from_sideline, outside_leverage)
  
  divider_all = pt_data %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId, nflId) %>%
    filter(cumsum(event == "ball_snap") >= 1) %>%
    group_by(gameId, playId, nflId) %>%
    filter(cumsum((event != "None")&(event != "ball_snap")) < 1) %>%
    group_by(gameId, playId, nflId) %>%
    filter(max(frameId) - min(frameId) <= 30) %>%
    inner_join(closest_pairs,
               by = c("gameId", "playId", "nflId")) %>%
    filter(frameId >= frameId_start,
           frameId <= frameId_end) %>%
    inner_join(pt_data %>%
                 dplyr::select(gameId, playId, frameId, nflId, x, y) %>%
                 rename(x_off = x,
                        y_off = y),
               by = c("gameId", "playId", "frameId", "nflId_opp" = "nflId")) %>%
    anti_join(within_the_25) %>%
    inner_join(snap_data) %>%
    inner_join(receiver_snap_data,
               by = c("gameId", "playId", "nflId_opp" = "nflId_snap_receiver")) %>%
    mutate(receiver_x_disp = x_off - x_snap_receiver,
           receiver_disp_towards_sideline = if_else(y_snap_receiver < 160/6, 
                                                    y_snap_receiver - y_off,
                                                    y_off - y_snap_receiver),
           receiver_dist_from_sideline = if_else(y_off < 160/6, 
                                                 y_off,
                                                 160/3 - y_off)) %>%
    mutate(separation = sqrt((x_off - x)^2 + (y_off - y)^2)) %>%
    # filter(separation <= 4) %>%
    mutate(sideline_distance_from_snap = if_else(y < 160/6,
                                                 y_snap,
                                                 160/3 - y_snap)) %>%
    ungroup() %>%
    mutate(outside_leverage = if_else(y < 160/6, 
                                      y_off - y,
                                      y - y_off),
           db_distance_from_sideline = if_else(y < 160/6, 
                                            y,
                                            160/3 - y)) %>%
    rename(nflId_off = nflId_opp) %>%
    dplyr::select(gameId, playId, frameId, nflId, nflId_off,separation,x,x_off,
                  receiver_x_disp, receiver_disp_towards_sideline,
                  receiver_dist_from_sideline,
                  sideline_distance_from_snap, db_distance_from_sideline, outside_leverage)
  
  
  
  divider_total = rbind.data.frame(divider_total,
                                   divider)
  
  divider_all_total = rbind.data.frame(divider_all_total,
                                       divider_all)
  
    write.csv(divider_total,
            "~/Desktop/CoverageNet/explore/outputs/divider_man_data.csv",
            row.names = FALSE)
    
    write.csv(divider_all_total,
              "~/Desktop/CoverageNet/explore/outputs/divider_all_data.csv",
              row.names = FALSE)
    
    print(file)
}

divider_all_total = read.csv("~/Desktop/CoverageNet/explore/outputs/divider_all_data.csv")

divider_all_total2 = divider_all_total %>%
  filter(x > x_off) %>%
  left_join(man_coverage,
            by = c("gameId", "playId", "nflId_off")) %>%
  filter(is.na(nflId_def)|(nflId == nflId_def)) %>%
  dplyr::select(-nflId_def)

divider_all_total3 = divider_all_total2 %>%
  filter(receiver_x_disp > 0,
         receiver_x_disp < 15) %>%
  filter(receiver_dist_from_sideline <= 15) %>%
  filter(abs(receiver_disp_towards_sideline) <= 7.5) %>%
  filter(receiver_x_disp > abs(receiver_disp_towards_sideline)) %>%
  filter(separation <= 4)

# visualizing the data

divider_all_total3 %>%
  ggplot() +
  geom_density(aes(x = outside_leverage), fill = "blue", alpha = .7)

divider_agg = divider_all_total3 %>%
  filter(sideline_distance_from_snap > 20,
         sideline_distance_from_snap < 32) %>%
  mutate(distance_from_sideline_disc = round(receiver_dist_from_sideline, 1)) %>%
  group_by(distance_from_sideline_disc) %>%
  summarize(count = n(),
            avg_outside_leverage = mean(outside_leverage)) %>%
  arrange(distance_from_sideline_disc)

divider_agg %>%
  filter(count > 100,
         distance_from_sideline_disc <= 12,
         distance_from_sideline_disc >= 5) %>%
  ggplot() +
  geom_point(aes(x = distance_from_sideline_disc, y = avg_outside_leverage),
             pch = 21, fill = "darkgray", color = "black") +
  geom_smooth(aes(x = distance_from_sideline_disc, y = avg_outside_leverage), method = "lm",
              se = FALSE, size = .75, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Defender Distance from Sideline (yards)",
       y = "Defender Outside Leverage (yards)",
       title = "Relationship Between Defender Outside Leverage and Distance From Sideline",
       subtitle = "Average divider employed is ~9.5 yards.")


divider_agg2 = divider_all_total2 %>%
  filter(x > x_off) %>%
  mutate(sideline_distance_from_snap_disc = case_when(sideline_distance_from_snap < 160/6 - 3 ~ '1 CLOSE',
                                                 sideline_distance_from_snap < 160/6 + 3 ~ '2 MIDDLE',
                                                 TRUE ~ '3 FAR')) %>%
  filter(sideline_distance_from_snap >= 20,
         sideline_distance_from_snap <= 32) %>%
  rowwise() %>%
  mutate(distance_from_sideline_disc = floor(receiver_dist_from_sideline*4)/4 + .125) %>%
  filter(distance_from_sideline_disc <= 12,
         distance_from_sideline_disc >= 5) %>%
  group_by(sideline_distance_from_snap_disc, distance_from_sideline_disc) %>%
  summarize(count = n(),
            avg_outside_leverage = mean(outside_leverage)) %>%
  arrange(distance_from_sideline_disc) %>%
  mutate(sideline_distance_from_snap_disc = paste0("Snap Location: ", sideline_distance_from_snap_disc))

plt_table = rbind.data.frame(
  divider_agg2 %>% distinct(sideline_distance_from_snap_disc) %>%
    mutate(x = 5, y = 0),
  do(divider_agg2 %>%
     filter(distance_from_sideline_disc <= 12,
            distance_from_sideline_disc >= 5) %>%
     group_by(sideline_distance_from_snap_disc), 
   tidy(lm(avg_outside_leverage ~ distance_from_sideline_disc, 
            data = .))) %>%
  group_by(sideline_distance_from_snap_disc) %>%
  summarize(x = -1*estimate[1]/estimate[2]) %>%
  dplyr::select(sideline_distance_from_snap_disc, x) %>%
  mutate(y = 0),
  do(divider_agg2 %>%
       filter(distance_from_sideline_disc <= 12,
              distance_from_sideline_disc >= 5) %>%
       group_by(sideline_distance_from_snap_disc), 
     tidy(lm(avg_outside_leverage ~ distance_from_sideline_disc, 
             data = .))) %>%
    group_by(sideline_distance_from_snap_disc) %>%
    summarize(x = -1*estimate[1]/estimate[2]) %>%
    dplyr::select(sideline_distance_from_snap_disc, x) %>%
    mutate(y = -1.3)) %>%
  ungroup() %>%
  dplyr::arrange(sideline_distance_from_snap_disc, x, desc(y))
  

divider_agg2 %>%
  filter(distance_from_sideline_disc <= 12,
         distance_from_sideline_disc >= 5) %>%
  ggplot() +
  geom_point(aes(x = distance_from_sideline_disc, y = avg_outside_leverage),
             pch = 21, fill = "darkgray", color = "black") +
  geom_smooth(aes(x = distance_from_sideline_disc, y = avg_outside_leverage), method = "lm",
              se = FALSE, size = .5, color = "blue") +
  geom_path(data = plt_table, aes(x = x, y = y), linetype = "dashed") +
  facet_wrap(~sideline_distance_from_snap_disc, ncol = 1) + 
  labs(x = "Defender Distance from Sideline (yards)",
       y = "Defender Outside Leverage (yards)",
       title = "Average Divider Distance from Sideline",
       subtitle = "The divider increases as the snap location moves further from the sideline.")

