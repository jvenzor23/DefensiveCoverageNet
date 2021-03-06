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


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv")
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
coverages_week1 = read.csv("~/Desktop/CoverageNet/inputs/coverages_week1.csv")

pbp_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week1.csv")

man_zone_classification = rbind(
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_pass_attempts_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_sacks_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability)
) %>%
  arrange(gameId, playId, nflId) %>%
  distinct(gameId, playId, nflId, .keep_all = TRUE)

man_coverage = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")

zone_coverage = man_zone_classification %>%
  anti_join(man_coverage,
            by = c("gameId", "playId", "nflId" = "nflId_def"))

man_assignments = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
zone_assignments = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")

# Making the line segments df ---------------------------------------------
man_assignments_segment_plt =  rbind(
  man_assignments %>%
  inner_join(pbp_data %>%
               dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
               rename(nflId_off = nflId,
                      x_off = x,
                      y_off = y)) %>%
  inner_join(pbp_data %>%
               dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
               rename(nflId_def = nflId,
                      x_def = x,
                      y_def = y)) %>%
    mutate(coverage_type = "man"),
  pbp_data %>%
    inner_join(zone_assignments) %>%
    filter(frameId >= frameId_start,
           frameId <= frameId_end) %>%
    dplyr::select(gameId, playId, nflId, nflId_opp, frameId, x, y) %>%
    rename(nflId_def = nflId,
           x_def = x,
           y_def = y) %>%
    inner_join(pbp_data,
               by = c("gameId", "playId", "frameId", "nflId_opp" = "nflId")) %>%
    rename(nflId_off = nflId_opp,
           x_off = x,
           y_off = y) %>%
    dplyr::select(gameId, playId, nflId_def, nflId_off, frameId, 
                  ends_with("def"), ends_with("off")) %>%
    mutate(coverage_type = "zone")
) %>%
  arrange(gameId, playId, frameId, nflId_def)

# Animating a Play --------------------------------------------------------

## Select Play
## 2018090912/2115 is an example of zone it gets right
## 2018090906/3594 is an example of a zone it gets wrong (but right for our purposes)
## 2018090900/4093 same as above
## 2018090901/4688 same as above
## 2018091001/1843 is an example of getting 2 zones correct and 1 wrong. Shows a need for
## recongnizing that no movement at all probably means zone! (had the most imp variable in
## his favor as well, confusing as to why this would assign zone)
## 2018090906/3223 seems just backwards --> CHECK THIS OUT!!! (maybe fumble related) (CHECK PROGRESSION HERE!)


## 2018090912/3849 an example of man coverage missed for one player because of traffic
## 2018090900/1260 another example of this! (maybe square numerator of ratio?!?!)
## 2018090905/1646 is another example, where 21 screws up the ratio metric

## could be solved by 1) pooling or 2) tracking classification at different stages (ie, after release)
## 2018090902/2316 needs to be fixed to detect zone

## KRYPTONITE FOR ZONE: 2018090910/3171 has Donte Jackson as man, because
## it cannot tell that he passed off 83 and then got 89, it just looks to the
## GMM like a continous flow with the same guy:
## track metric like "max % of time closest to specific offensive player"
## 2018090902/1302 #22 another example of this, passes off 11 for 84,
## algorithm cannot tell
## Also can get max correlation x/y direction, and average them
## crazy example of where correlation on all offensive players goes wrong!
## ****** TO DO TOMORROW: and then check that all the favorable results are
## maintained
## - solution: get correlation of motion with closest player (or one with
## same direction!!!!)

## KRYPTONIE FOR MAN: 2018090903/1336

## NEED TO ACCOUNT FOR BLITZ!!!!: 2018090903/2381
## 2018090910/676 as well!

## 2018090905/337: really interesting example of trips, with passing offf
## and covering man whoever comes to each side! CODE WORKED HERE, WANT TO SEE
## BROKEN DOWN BY POSSESSION INTERVAL!!!!

## ******************
## Check 2018091000/2177 by possession interval as well (22)
## issue here looks like the crowded beginning is throwing off the ratio
## could add a buffer to improve this behavior
## 2018090903/2597 as well exhibits this same behavior
## *****************

## and 2018091000/3393
## and 2018090903/596 (28 must be in man after the start)
## and 2018090909/3297
## and 2018090902/1815 (#21)

## 2018090907/936 is a literal perfect example of how this tagging works
## 2018091000/785 another great example
# **********2018090907/1382 is the perfect example!!!!!***********
# **********2018090907/3430 is another perfect example!!!*********
# **********2018090901/4596 another perfect example of how it gets the
# priciples correct, not necessarily the exact coverage
# **********2018090900/1260 also example of getting principles correct
## 2018090906/2748 shows 2 ways this method can go wrong:
## - 21: limited variation in a direction by WR leads to error in cors
## - 20: CB falling down or stopping once ball is thrown makes it look like zone

## 2018090901/2885 shows the same issue as above with limited variablity:
## what if we did a coord switch for corr to ensure adequate variablitiy in both dirs?!?!?
## use #28 as an example here!
## ****** SOLUTION: ROTATE about ball_snap point 45˚, and take maximum!!!!!!!! ***********


## FOR SAFETIES: 2018090912/3849 great example of code working!
## 2018090910/477 another good one!zone_coverage
example.play = pbp_data %>%
  inner_join(
    pbp_data %>%
      # dplyr::select(gameId, playId) %>%
      filter(gameId == 2018090600,
           playId == 1715) %>%
      distinct()
      # sample_n(1)
  )

example_man_assignments_segment_plt = man_assignments_segment_plt %>%
  inner_join(example.play %>%
               distinct(gameId, playId))

example_zones = example.play %>%
  inner_join(zone_coverage %>%
              distinct(gameId, playId, nflId))

example_rushers = example.play %>%
  filter(!is.na(nflId),
         !IsOnOffense) %>%
  anti_join(zone_coverage %>%
              distinct(gameId, playId, nflId)) %>%
  anti_join(man_coverage %>%
              distinct(gameId, playId, nflId_def) %>%
              rename(nflId = nflId_def))

example.play.info = plays %>%
  inner_join(example.play %>%
               dplyr::select(gameId, playId) %>%
               distinct()) %>%
  inner_join(targeted_receiver) %>%
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
  geom_point(data = example_zones, aes(x = (xmax-y), y = x + 10, group = nflId), fill = "blue", color = "white",  alpha = 0.5, size = 10) +
  geom_point(data = example_rushers, aes(x = (xmax-y), y = x + 10, group = nflId), fill = "blue", color = "red",  alpha = 0.5, size = 10) +
  geom_point(data = example.play, aes(x = (xmax-y), y = x + 10, shape = team,
                                      fill = team, group = nflId, size = team, colour = team), alpha = 0.9) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x + 10, label = jerseyNumber, group = nflId), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  geom_segment(aes(x = (xmax - y_off), y = x_off + 10, xend = (xmax - y_def), yend = x_def + 10, group = nflId_def, 
                   linetype = coverage_type), 
                color = "black", size = 1, data = example_man_assignments_segment_plt) + 
  geom_point(data = example.play %>%
               filter(is.na(nflId)), aes(x = (xmax-y), y = x + 10), 
             fill = "brown", color = "#654321", shape = 16, size = 4,
             alpha = 1) + 
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(55),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), color = "limegreen",  alpha = 0.5, size = 14) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(55),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), fill = "blue", color = "white",  alpha = 0.7, size = 10) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(55),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), color = "#002244",  alpha = 0.9, size = 6) +
  geom_text(data = data.frame("x" = c(-12.5),
                                     "y" = c(55),
                                     "text" = c("Zone Coverage"),
                                     "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(45),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), fill = "blue", color = "red",  alpha = 0.5, size = 10) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(45),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), color = "#002244",  alpha = 0.9, size = 6) +
  geom_text(data = data.frame("x" = c(-17),
                              "y" = c(45),
                              "text" = c("Rushing"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  geom_segment(data = data.frame("x" = c(-26.5),
                                       "y" = c(35),
                                       "xmax" = c(-23.5),
                                       "dummy" = c(1)) %>%
                 full_join(example.play %>%
                             distinct(frameId) %>%
                             mutate(dummy = 1)),
               aes(x = x, y = y, xend = xmax, yend = y), color = "black", size = 1,
               linetype = "solid") +
  geom_text(data = data.frame("x" = c(-12.5),
                              "y" = c(35),
                              "text" = c("Man Assignment"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  geom_segment(data = data.frame("x" = c(-26.5),
                                 "y" = c(25),
                                 "xmax" = c(-23.5),
                                 "dummy" = c(1)) %>%
                 full_join(example.play %>%
                             distinct(frameId) %>%
                             mutate(dummy = 1)),
               aes(x = x, y = y, xend = xmax, yend = y), color = "black", size = .5,
               linetype = "dashed") +
  geom_text(data = data.frame("x" = c(-12.5),
                              "y" = c(25),
                              "text" = c("Zone Assignment"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  ylim(ymin, ymax) + 
  labs(title = paste0(game.info$visitorTeamAbbr, " @ ", game.info$homeTeamAbbr,
                      " (", game.info$gameDate, ")"),
       subtitle = trimws(paste0("Down and Distance: ", toString(example.play.info$DownDesc), "\n", "\n",
                        paste(strwrap(paste("Play Description:", toString(example.play.info$playDescription))), collapse="\n")))) +
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
  # labs(title = toString(example.play.info$coverage),
  #      subtitle = paste0(toString(example.play.info$playDescription),
  #                        "\n", "EPA = ", toString(round(example.play.info$epa, 2)))) +
  transition_time(frameId)  +
  ease_aes('linear')

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frameId))
animate(animate.play, fps = 10, nframe = play.length.ex)


ggplot() +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(55),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), color = "limegreen",  alpha = 0.5, size = 14) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(55),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), fill = "blue", color = "white",  alpha = 0.7, size = 10) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(55),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), color = "#002244",  alpha = 0.9, size = 6) +
  geom_text(data = data.frame("x" = c(-16),
                              "y" = c(55),
                              "text" = c("Zone Coverage"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(45),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), fill = "blue", color = "red",  alpha = 0.5, size = 10) +
  geom_point(data = data.frame("x" = c(-25), 
                               "y" = c(45),
                               "dummy" = c(1)) %>%
               full_join(example.play %>% 
                           distinct(frameId) %>%
                           mutate(dummy = 1)),
             aes(x = x, y = y), color = "#002244",  alpha = 0.9, size = 6) +
  geom_text(data = data.frame("x" = c(-18),
                              "y" = c(45),
                              "text" = c("Rushing"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  geom_segment(data = data.frame("x" = c(-26.5),
                                 "y" = c(35),
                                 "xmax" = c(-23.5),
                                 "dummy" = c(1)) %>%
                 full_join(example.play %>%
                             distinct(frameId) %>%
                             mutate(dummy = 1)),
               aes(x = x, y = y, xend = xmax, yend = y), color = "black", size = 1,
               linetype = "solid") +
  geom_text(data = data.frame("x" = c(-15.5),
                              "y" = c(35),
                              "text" = c("Man Assignment"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) +
  geom_segment(data = data.frame("x" = c(-26.5),
                                 "y" = c(25),
                                 "xmax" = c(-23.5),
                                 "dummy" = c(1)) %>%
                 full_join(example.play %>%
                             distinct(frameId) %>%
                             mutate(dummy = 1)),
               aes(x = x, y = y, xend = xmax, yend = y), color = "black", size = .5,
               linetype = "dashed") +
  geom_text(data = data.frame("x" = c(-15.5),
                              "y" = c(25),
                              "text" = c("Zone Assignment"),
                              "dummy" = c(1)) %>%
              full_join(example.play %>%
                          distinct(frameId) %>%
                          mutate(dummy = 1)),
            aes(x = x, y = y, label = text), colour = "black",
            vjust = 0.36, size = 4.5) + 
  xlim(-30, -5) +
  ylim(22, 58) +
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
        plot.background=element_blank())

ggsave("~/Desktop/CoverageNet/src/08_writeup/images/CoverageKey2.png",
       height = 6,
       width = 6) 
