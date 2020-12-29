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

# Classifying Coverages by Position ---------------------------------------

total_coverage = rbind(
  man_coverage %>% dplyr::select(gameId, playId, nflId_def) %>%
    rename(nflId = nflId_def) %>%
    mutate(type = "man"),
  zone_coverage %>% dplyr::select(gameId, playId, nflId) %>%
    mutate(type = "zone")
) %>%
  inner_join(players %>%
               dplyr::select(nflId, position))

coverage_by_position_agg = total_coverage %>%
  mutate(position_disc = case_when(grepl("s", str_to_lower(position)) ~ "S",
                                   grepl("lb", str_to_lower(position)) ~ "LB",
                                   grepl("b", str_to_lower(position)) ~ "CB",
                                   TRUE ~ "remove"
                                         )) %>%
  filter(position_disc != "remove") %>%
  group_by(position_disc) %>%
  summarize(players = length(unique(nflId)),
            plays = n(),
            perc_man = mean(type == "man")) %>%
  mutate(defense_disc = "Overall")


# Breaking Up By Defensive Coverages --------------------------------------

unique(coverages_week1$coverage)

coverage_by_defense_position_agg = total_coverage %>%
  mutate(position_disc = case_when(grepl("s", str_to_lower(position)) ~ "S",
                                   grepl("lb", str_to_lower(position)) ~ "LB",
                                   grepl("b", str_to_lower(position)) ~ "CB",
                                   TRUE ~ "remove"
  )) %>%
  filter(position_disc != "remove") %>%
  inner_join(coverages_week1) %>%
  mutate(defense_disc = if_else(grepl("Man", coverage),
                                "Man Coverage Call",
                                "Zone Coverage Call")) %>%
  group_by(defense_disc, position_disc) %>%
  summarize(players = length(unique(nflId)),
            plays = n(),
            perc_man = mean(type == "man")) %>%
  arrange(defense_disc, position_disc)


# Plotting the Data -------------------------------------------------------

coverage_summary_plot = rbind.data.frame(coverage_by_position_agg,
                              coverage_by_defense_position_agg)

my_theme = theme_minimal() +
  theme(plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        axis.title = element_text(size=10),
        legend.title = element_text(size=10))

ggplot() +
  # scale_fill_manual(values = c("dodgerblue3", "orangered2")) +
  geom_bar(data = coverage_by_defense_position_agg,
           aes(x = position_disc,
               y = perc_man,
               fill = defense_disc),
           color = "black",
           size = .2,
           stat = "identity",
           position = position_dodge(width=0.5),
           width = .45) +
  labs(x = "Position Group",
       y = "% of Plays in Man Coverage",
       title = "Coverage Classification By Position Group and Defensive Call",
       subtitle = "Week 1 data only (tagged defensive coverages from week 1 were shared)",
       fill = "Defensive Coverage") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, .8)) +
  my_theme

ggsave("~/Desktop/CoverageNet/src/08_writeup/images/PositionDefenseCoverageDist.png",
       height = 3,
       width = 6)

# Vizualizing A Zone Play -------------------------------------------------

# man example: 2018090600/1715
# zone example: 2018090907/3430  
library(gifski)
# animating the man coverage play
source("~/Desktop/CoverageNet/src/01_identify_man_coverage/funs/animate_coverages.R")
man.coverage.play.info = animate_coverages(week = 1,
                                           gameId_ex = 2018090600,
                                           playId_ex = 1715)

animate.man.play = man.coverage.play.info$animate.play
man.play.length.ex = man.coverage.play.info$play.length.ex

animate(animate.man.play, fps = 10, nframe = man.play.length.ex)
man_gif = animate(animate.man.play, fps = 10, nframe = man.play.length.ex)
animate(animate.man.play, 
        nframes = man.play.length.ex, device = "png", fps = 10,
        renderer = file_renderer("~/Desktop/CoverageNet/src/08_writeup/images/ManCoverageEx.png", 
                                 prefix = "gganim_plot", overwrite = TRUE),
        height = 800,
        width = 800,
        res = 120)

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/ManCoverageEx.gif",
          animate.man.play,
          fps = 10,
          nframe = man.play.length.ex,
          height = 600,
          width = 600,
          res = 120)

# animating the zone coverage play
zone.coverage.play.info = animate_coverages(week = 1,
                                           gameId_ex = 2018090907,
                                           playId_ex = 3430)

animate.zone.play = zone.coverage.play.info$animate.play
zone.play.length.ex = zone.coverage.play.info$play.length.ex

animate(animate.zone.play, fps = 10, nframe = zone.play.length.ex)
zone_gif = animate(animate.zone.play, fps = 10, nframe = zone.play.length.ex)
animate(animate.zone.play, 
        nframes = zone.play.length.ex, device = "png", fps = 10,
        renderer = file_renderer("~/Desktop/CoverageNet/src/08_writeup/images/ZoneCoverageEx.png", 
                                 prefix = "gganim_plot", overwrite = TRUE),
        height = 800,
        width = 800,
        res = 120)

anim_save("~/Desktop/CoverageNet/src/08_writeup/images/ZoneCoverageEx.gif",
          animate.zone.play,
          fps = 10,
          nframe = zone.play.length.ex,
          height = 800,
          width = 800,
          res = 120)

man_mgif <- image_read(man_gif)
zone_mgif <- image_read(zone_gif)

man_mgif[1]

new_gif <- image_append(c(man_mgif[1], zone_mgif[1]))
for(i in 2:zone.play.length.ex){
  if(i > man.play.length.ex){
    combined <- image_append(c(man_mgif[man.play.length.ex], zone_mgif[i]))
  }else{
    combined <- image_append(c(man_mgif[i], zone_mgif[i]))
  }
  new_gif <- c(new_gif, combined)
}

new_gif


anim_save("~/Desktop/CoverageNet/src/08_writeup/images/CoverageEx.gif",
          new_gif,
          fps = 10,
          nframe = zone.play.length.ex,
          height = 400,
          width = 600,
          res = 100)

  
  
  