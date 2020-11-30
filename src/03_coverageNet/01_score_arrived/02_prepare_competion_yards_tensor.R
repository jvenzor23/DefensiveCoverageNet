# This code prepares the tensor for completion yards prediction

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

set.seed(8451)
# Reading in The Data -----------------------------------------------------

pass_attempts = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)

# finding plays without dir
plays_remove = pass_attempts %>%
  filter(!is.na(nflId)) %>%
  filter(is.na(dir)) %>%
  distinct(gameId, playId)

plays_remove2 = pass_attempts %>%
  group_by(gameId, playId) %>%
  summarize(receivers = sum(IsReceiver)) %>%
  filter(receivers != 1)

# need to remove duplicates of plays with multiple events
plays_remove3 = pass_attempts %>%
  group_by(gameId, playId) %>%
  filter(frameId != min(frameId)) %>%
  distinct(gameId, playId, frameId)

# need to remove duplicates of plays with no football!
plays_remove4 = pass_attempts %>%
  group_by(gameId, playId) %>%
  filter(sum(is.na(nflId)) == 0) %>%
  distinct(gameId, playId)

pass_attempts = pass_attempts %>%
  anti_join(plays_remove) %>%
  anti_join(plays_remove2) %>%
  anti_join(plays_remove3) %>%
  anti_join(plays_remove4)

# ensuring a completion
pass_attempts = pass_attempts %>%
  inner_join(plays %>%
               filter(passResult == 'C') %>%
               distinct(gameId, playId))

check = pass_attempts %>%
  distinct(gameId, playId)

# Sampling to Get Training, Validation, Test ------------------------------

pass_attempts_holdout_plays = pass_attempts %>%
  select(gameId, playId) %>%
  distinct() %>%
  sample_frac(0.3)

pass_attempts_validation_plays = pass_attempts_holdout_plays %>%
  sample_frac(0.5)

pass_attempts_test_plays = pass_attempts_holdout_plays %>%
  anti_join(pass_attempts_validation_plays)


# Adding in Extra Plays By Flipping ---------------------------------------
pass_attempts_flipped = pass_attempts %>%
  anti_join(pass_attempts_holdout_plays) %>%
  mutate(y = 53 + 1/3 - y,
         dir = 180 - dir) %>%
  mutate(gameId = gameId + 10000000000)


pass_attempts_tot = rbind(pass_attempts %>%
                             anti_join(pass_attempts_holdout_plays),
                           pass_attempts_flipped) %>%
  mutate(group = "training")

pass_attempts_validation_flipped = pass_attempts %>%
  inner_join(pass_attempts_validation_plays) %>%
  mutate(y = 53 + 1/3 - y,
         dir = 180 - dir) %>%
  mutate(gameId = gameId + 20000000000)

pass_attempts_validation_tot = rbind(pass_attempts %>%
                                        inner_join(pass_attempts_validation_plays),
                                      pass_attempts_validation_flipped) %>%
  mutate(group = "validation")

pass_attempts_test_flipped = pass_attempts %>%
  inner_join(pass_attempts_test_plays) %>%
  mutate(y = 53 + 1/3 - y,
         dir = 180 - dir) %>%
  mutate(gameId = gameId + 20000000000)

pass_attempts_test_tot = rbind(pass_attempts %>%
                                  inner_join(pass_attempts_test_plays),
                                pass_attempts_test_flipped) %>%
  mutate(group = "test")


pass_attempts_tot = rbind(pass_attempts_tot,
                           pass_attempts_validation_tot,
                           pass_attempts_test_tot)

# Deriving Features for Neural Network ------------------------------------

# Features:
# ------------------ FROM KAGGLE WINNERS ------------------------
# def S(x,y)
# receiver S(x,y)
# def (x,y) - receiver (x,y)
# def S(x,y) - receiver S(x,y)
# def (x,y) - football (x,y)
# off (x,y) - def (x,y)
# off S(x,y) - def S(x,y)
# distance of WR down the field
# football speed (?)
# ---------------------------------------------------------------

# computing S(x,y)
pass_attempts_tot2 = pass_attempts_tot %>%
  mutate(s_x = s*cos((90-dir)*pi/180),
         s_y = s*sin((90-dir)*pi/180)) %>%
  select(group, gameId, playId, nflId, x, y, s, s_x, s_y, IsOnOffense, IsReceiver, 
         YardsFromOwnGoal)

# ball carrier run data
targeted_wr_data = pass_attempts_tot2 %>%
  filter(IsReceiver) %>%
  arrange(gameId, playId) %>%
  rename(receiver_x = x,
         receiver_y = y,
         receiver_s_x = s_x,
         receiver_s_y = s_y) %>%
  select(-IsOnOffense, -IsReceiver, -s) %>%
  mutate(receiver_distance_from_los = receiver_x - YardsFromOwnGoal)

offense_data = pass_attempts_tot2 %>%
  filter(IsOnOffense & (!IsReceiver)) %>%
  arrange(gameId, playId, nflId) %>%
  rename(offense_x = x,
         offense_y = y,
         offense_s_x = s_x,
         offense_s_y = s_y,
         offense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver, -s)

defense_data = pass_attempts_tot2 %>%
  filter(!IsOnOffense & (!is.na(nflId))) %>%
  arrange(gameId, playId, nflId) %>%
  rename(defense_x = x,
         defense_y = y,
         defense_s_x = s_x,
         defense_s_y = s_y,
         defense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver, -s)

football_data = pass_attempts_tot2 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId) %>%
  rename(football_x = x,
         football_y = y,
         football_s = s) %>%
  select(gameId, playId, starts_with("football"))

# getting defense vs. rusher stats
tot_data1 = defense_data %>%
  inner_join(targeted_wr_data %>% select(-nflId, -YardsFromOwnGoal), by = c("gameId", "playId")) %>%
  inner_join(football_data, by = c("gameId", "playId")) %>%
  mutate(defense_receiver_x = defense_x - receiver_x,
         defense_receiver_y = defense_y - receiver_y,
         defense_receiver_s_x = defense_s_x - receiver_s_x,
         defense_receiver_s_y = defense_s_y - receiver_s_y,
         defense_football_x = defense_x - football_x,
         defense_football_y = defense_y - football_y) %>%
  select(-starts_with("receiver"), receiver_s_x, receiver_s_y, receiver_distance_from_los,
         -football_x, -football_y)

# getting defense vs. offense stats
tot_data2 = tot_data1 %>%
  inner_join(offense_data, by = c("gameId", "playId")) %>%
  mutate(defense_offense_x = defense_x - offense_x,
         defense_offense_y = defense_y - offense_y,
         defense_offense_s_x = defense_s_x - offense_s_x,
         defense_offense_s_y = defense_s_y - offense_s_y
  ) %>%
  arrange(gameId, playId, defense_nflId, offense_nflId) %>%
  select(group, gameId, playId, defense_nflId, offense_nflId, 
         defense_s_x, defense_s_y,
         defense_receiver_x, defense_receiver_y,
         defense_receiver_s_x, defense_receiver_s_y,
         defense_offense_x, defense_offense_y,
         defense_offense_s_x, defense_offense_s_y,
         receiver_s_x, receiver_s_y, receiver_distance_from_los,
         defense_football_x, defense_football_y, football_s)
  
# adjusting nflIds to 1-10/1-11
tot_data3 = tot_data2 %>%
  group_by(group, gameId, playId) %>%
  mutate(defenseId = dense_rank(defense_nflId),
         offenseId = dense_rank(offense_nflId)) %>%
  ungroup() 

numbered_plays = tot_data3 %>%
  distinct(group, gameId, playId) %>%
  arrange(group, gameId, playId) %>%
  group_by(group) %>%
  mutate(playId2 = row_number())

tot_data3 = tot_data3 %>%
  inner_join(numbered_plays) %>%
  arrange(group, gameId, playId2, defense_nflId, offense_nflId) %>%
  select(group, gameId, playId, defense_nflId, offense_nflId, playId2, defenseId, offenseId, everything())

# Adding in Padding to Each Play ------------------------------------------

max_def_players = max(tot_data3$defenseId)
max_off_players = max(tot_data3$offenseId)

check_off_dist = tot_data3 %>%
  group_by(group, gameId, playId) %>%
  summarize(num_off = max(offenseId)) %>%
  ungroup() %>%
  group_by(num_off) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

check_def_dist = tot_data3 %>%
  group_by(group, gameId, playId) %>%
  summarize(num_def = max(defenseId)) %>%
  ungroup() %>%
  group_by(num_def) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

n_off_df = data.frame("offenseId" = seq(1, max_off_players), "dummy" = 1)
n_def_df = data.frame("defenseId" = seq(1, max_def_players), "dummy" = 1)

padding_df = n_off_df %>%
  full_join(n_def_df) %>%
  full_join(tot_data3 %>%
              distinct(group, gameId, playId, playId2) %>%
              mutate(dummy = 1)) %>%
  select(group, gameId, playId, defenseId, offenseId, playId2) %>%
  arrange(group, gameId, playId, defenseId, offenseId, playId2)

tot_data4 = padding_df %>%
  left_join(tot_data3) %>%
  arrange(group, gameId, playId2, defenseId, offenseId) %>%
  ungroup() %>%
  select(group, gameId, playId, defense_nflId, offense_nflId, playId2, defenseId, offenseId, everything())

tot_data4[is.na(tot_data4)] = 0

# Getting the Target ------------------------------------------------------
target = tot_data4 %>%
  select(group, gameId, playId, playId2) %>%
  inner_join(pass_attempts_tot %>%
               distinct(gameId, playId, offensePlayResult)) %>%
  distinct() %>%
  arrange(group, gameId, playId2) %>%
  mutate(offensePlayResult_adj = case_when(offensePlayResult < -10 ~ as.numeric(-10),
                                           offensePlayResult > 50 ~ as.numeric(50),
                                           TRUE ~ as.numeric(offensePlayResult)))


# Adding in Overall PlayId for total scoring! -----------------------------

numbered_plays2 = target %>%
  distinct(gameId, playId) %>%
  arrange(gameId, playId) %>%
  mutate(playId3 = row_number())

tot_data5 = tot_data4 %>%
  inner_join(numbered_plays2) %>%
  select(group, gameId, playId, defense_nflId, offense_nflId, playId3, playId2, defenseId, offenseId, everything())

target2 = target %>%
  inner_join(numbered_plays2) %>%
  select(group, gameId, playId, playId2, playId3, everything())

setwd("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/")
write.csv(tot_data5, "pass_complete_training_tensor.csv", row.names = FALSE)
write.csv(target2, "pass_complete_training_target.csv", row.names = FALSE)


# Plotting the Data -------------------------------------------------------


passes_test_plt = pass_attempts %>%
  inner_join(target %>%
               filter(group == "test",
                      gameId < 20000000000) %>%
               distinct(gameId, playId))

# play that went for neg yards:
plt_neg = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(offensePlayResult < 0) %>%
               select(gameId, playId) %>%
               distinct() %>%
               filter(row_number() == 4)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense"))


ggplot(colour = "black")  + 
  geom_point(data = plt_neg %>% filter(!is.na(nflId)), 
             aes(x, y, fill = grouping), size = 4, pch = 21) + 
  geom_point(data = plt_neg %>% filter(is.na(nflId)), 
             aes(x, y), fill = 'brown', size = 3, pch = 21) + 
  geom_segment(data = plt_neg %>% filter(!is.na(nflId)),
               aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(data = plt_neg %>% filter(!is.na(nflId)),
             aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(plt_neg$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

plt_0 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(offensePlayResult == 0) %>%
               select(gameId, playId) %>%
               distinct() %>%
               filter(row_number() == 2)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense"))


ggplot(colour = "black")  + 
  geom_point(data = plt_0 %>% filter(!is.na(nflId)), 
             aes(x, y, fill = grouping), size = 4, pch = 21) + 
  geom_point(data = plt_0 %>% filter(is.na(nflId)), 
             aes(x, y), fill = 'brown', size = 3, pch = 21) + 
  geom_segment(data = plt_0 %>% filter(!is.na(nflId)),
               aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(data = plt_0 %>% filter(!is.na(nflId)),
             aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(plt_0$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

plt_10 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(offensePlayResult == 10) %>%
               select(gameId, playId) %>%
               distinct() %>%
               filter(row_number() == 5)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense"))


ggplot(colour = "black")  + 
  geom_point(data = plt_10 %>% filter(!is.na(nflId)), 
             aes(x, y, fill = grouping), size = 4, pch = 21) + 
  geom_point(data = plt_10 %>% filter(is.na(nflId)), 
             aes(x, y), fill = 'brown', size = 3, pch = 21) + 
  geom_segment(data = plt_10 %>% filter(!is.na(nflId)),
               aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(data = plt_10 %>% filter(!is.na(nflId)),
             aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(plt_10$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

plt_30 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(offensePlayResult == 30) %>%
               select(gameId, playId) %>%
               distinct() %>%
               filter(row_number() == 4)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense"))


ggplot(colour = "black")  + 
  geom_point(data = plt_30 %>% filter(!is.na(nflId)), 
             aes(x, y, fill = grouping), size = 4, pch = 21) + 
  geom_point(data = plt_30 %>% filter(is.na(nflId)), 
             aes(x, y), fill = 'brown', size = 3, pch = 21) + 
  geom_segment(data = plt_30 %>% filter(!is.na(nflId)),
               aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(data = plt_30 %>% filter(!is.na(nflId)),
             aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(plt_30$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

plt_30 = pass_attempts %>%
  filter(gameId == 2018120210,
         playId == 3716) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y) %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense"))

ggplot(colour = "black")  + 
  geom_point(data = plt_30 %>% filter(!is.na(nflId)), 
             aes(x, y, fill = grouping), size = 4, pch = 21) + 
  geom_point(data = plt_30 %>% filter(is.na(nflId)), 
             aes(x, y), fill = 'brown', size = 3, pch = 21) + 
  geom_segment(data = plt_30 %>% filter(!is.na(nflId)),
               aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(data = plt_30 %>% filter(!is.na(nflId)),
             aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(plt_30$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

