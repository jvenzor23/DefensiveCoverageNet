# This prepares the tensor for making predictions of YAC 
# after the result of a catch.

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

pass_reception = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/pass_attempts.csv")

# finding plays without dir
plays_remove = pass_reception %>%
  filter(is.na(dir)) %>%
  distinct(gameId, playId)

plays_remove2 = pass_reception %>%
  group_by(gameId, playId) %>%
  summarize(receivers = sum(IsReceiver)) %>%
  filter(receivers != 1)

pass_reception = pass_reception %>%
  anti_join(plays_remove) %>%
  anti_join(plays_remove2)

check = pass_reception %>%
  distinct(gameId, playId)

# Sampling to Get Training, Validation, Test ------------------------------

pass_reception_holdout_plays = pass_reception %>%
  select(gameId, playId) %>%
  distinct() %>%
  sample_frac(0.3)

pass_reception_validation_plays = pass_reception_holdout_plays %>%
  sample_frac(0.5)

pass_reception_test_plays = pass_reception_holdout_plays %>%
  anti_join(pass_reception_validation_plays)


# Adding in Extra Plays By Flipping ---------------------------------------
pass_reception_flipped = pass_reception %>%
  anti_join(pass_reception_holdout_plays) %>%
  mutate(y = 53 + 1/3 - y,
         dir = 180 - dir) %>%
  mutate(gameId = gameId + 10000000000)


pass_reception_tot = rbind(pass_reception %>%
                             anti_join(pass_reception_holdout_plays),
                           pass_reception_flipped) %>%
  mutate(group = "training")

pass_reception_validation_flipped = pass_reception %>%
  inner_join(pass_reception_validation_plays) %>%
  mutate(y = 53 + 1/3 - y,
         dir = 180 - dir) %>%
  mutate(gameId = gameId + 20000000000)

pass_reception_validation_tot = rbind(pass_reception %>%
                             inner_join(pass_reception_validation_plays),
                           pass_reception_validation_flipped) %>%
  mutate(group = "validation")

pass_reception_test_flipped = pass_reception %>%
  inner_join(pass_reception_test_plays) %>%
  mutate(y = 53 + 1/3 - y,
         dir = 180 - dir) %>%
  mutate(gameId = gameId + 20000000000)

pass_reception_test_tot = rbind(pass_reception %>%
                                        inner_join(pass_reception_test_plays),
                                      pass_reception_test_flipped) %>%
  mutate(group = "test")


pass_reception_tot = rbind(pass_reception_tot,
                           pass_reception_validation_tot,
                           pass_reception_test_tot)

# Deriving Features for Neural Network ------------------------------------

# Features:
# ------------------ FROM KAGGLE WINNERS ------------------------
# def S(x,y)
# def (x,y) - receiver (x,y)
# def S(x,y) - receiver S(x,y)
# off (x,y) - def (x,y)
# off S(x,y) - def S(x,y)
# ---------------------------------------------------------------

# computing S(x,y)
pass_reception_tot2 = pass_reception_tot %>%
  mutate(s_x = s*cos((90-dir)*pi/180),
         s_y = s*sin((90-dir)*pi/180)) %>%
  select(group, gameId, playId, nflId, x, y, s_x, s_y, IsOnOffense, IsReceiver)

# ball carrier run data
wr_run_data = pass_reception_tot2 %>%
  filter(IsReceiver) %>%
  arrange(gameId, playId) %>%
  rename(receiver_x = x,
         receiver_y = y,
         receiver_s_x = s_x,
         receiver_s_y = s_y) %>%
  select(-IsOnOffense, -IsReceiver)

offense_run_data = pass_reception_tot2 %>%
  filter(IsOnOffense & (!IsReceiver)) %>%
  arrange(gameId, playId, nflId) %>%
  rename(offense_x = x,
         offense_y = y,
         offense_s_x = s_x,
         offense_s_y = s_y,
         offense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver)

defense_run_data = pass_reception_tot2 %>%
  filter(!IsOnOffense) %>%
  arrange(gameId, playId, nflId) %>%
  rename(defense_x = x,
         defense_y = y,
         defense_s_x = s_x,
         defense_s_y = s_y,
         defense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver)

# getting defense vs. rusher stats
tot_data1 = defense_run_data %>%
  inner_join(wr_run_data %>% select(-nflId), by = c("gameId", "playId")) %>%
  mutate(defense_receiver_x = defense_x - receiver_x,
         defense_receiver_y = defense_y - receiver_y,
         defense_receiver_s_x = defense_s_x - receiver_s_x,
         defense_receiver_s_y = defense_s_y - receiver_s_y
  ) %>%
  select(-starts_with("receiver"))

# getting defense vs. offense stats
tot_data2 = tot_data1 %>%
  inner_join(offense_run_data, by = c("gameId", "playId")) %>%
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
         defense_offense_s_x, defense_offense_s_y)

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
  inner_join(pass_reception_tot %>%
               distinct(gameId, playId, yards_after_catch)) %>%
  distinct() %>%
  arrange(group, gameId, playId2) %>%
  mutate(yards_after_catch_adj = case_when(yards_after_catch < -10 ~ as.numeric(-10),
                                           yards_after_catch > 50 ~ as.numeric(50),
                                           TRUE ~ as.numeric(yards_after_catch)))


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

setwd("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/")
write.csv(tot_data5, "yac_tensor.csv", row.names = FALSE)
write.csv(target2, "yac_target.csv", row.names = FALSE)

# Visualizing Some Test Plays ---------------------------------------

passes_test_plt = pass_reception %>%
                      inner_join(target %>%
                                 filter(group == "test",
                                        gameId < 20000000000) %>%
                                 distinct(gameId, playId))

# play that went for neg yards:
plt_neg = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(yards_after_catch < 0) %>%
               select(gameId, playId) %>%
               filter(row_number() == 1)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

plt_neg %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
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


# play that went for 0 yards:
plt_0 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(yards_after_catch == 0) %>%
               select(gameId, playId) %>%
               filter(row_number() == 20)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

plt_0 %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
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


# play that went for 5 yards:
plt_5 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(yards_after_catch == 5) %>%
               select(gameId, playId) %>%
               filter(row_number() == 1)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

plt_5 %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(plt_5$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

# play that went for 10 yards:
plt_10 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(yards_after_catch == 10) %>%
               select(gameId, playId) %>%
               filter(row_number() == 100)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

plt_10 %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
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

# play that went for 30+ yards:
plt_30 = passes_test_plt %>%
  inner_join(passes_test_plt %>%
               filter(yards_after_catch >= 30) %>%
               select(gameId, playId) %>%
               filter(row_number() == 20)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

plt_30 %>%
  mutate(grouping = case_when(IsReceiver ~ "Receiver",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
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


# Creating a Full Data Set to Score (all non-endzone catches) -------------
# NOTE: this data sets includes plays with a fumble

pass_reception_all = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/pass_attempts_all.csv")

# Adding in Extra Plays By Flipping ---------------------------------------
pass_reception_tot = pass_reception_all %>%
  mutate(group = "full")

# Deriving Features for Neural Network ------------------------------------

# Features:
# ------------------ FROM KAGGLE WINNERS ------------------------
# def S(x,y)
# def (x,y) - receiver (x,y)
# def S(x,y) - receiver S(x,y)
# off (x,y) - def (x,y)
# off S(x,y) - def S(x,y)
# ---------------------------------------------------------------

# computing S(x,y)
pass_reception_tot2 = pass_reception_tot %>%
  mutate(s_x = s*cos((90-dir)*pi/180),
         s_y = s*sin((90-dir)*pi/180)) %>%
  select(group, gameId, playId, nflId, x, y, s_x, s_y, IsOnOffense, IsReceiver)

# ball carrier run data
wr_run_data = pass_reception_tot2 %>%
  filter(IsReceiver) %>%
  arrange(gameId, playId) %>%
  rename(receiver_x = x,
         receiver_y = y,
         receiver_s_x = s_x,
         receiver_s_y = s_y) %>%
  select(-IsOnOffense, -IsReceiver)

offense_run_data = pass_reception_tot2 %>%
  filter(IsOnOffense & (!IsReceiver)) %>%
  arrange(gameId, playId, nflId) %>%
  rename(offense_x = x,
         offense_y = y,
         offense_s_x = s_x,
         offense_s_y = s_y,
         offense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver)

defense_run_data = pass_reception_tot2 %>%
  filter(!IsOnOffense) %>%
  arrange(gameId, playId, nflId) %>%
  rename(defense_x = x,
         defense_y = y,
         defense_s_x = s_x,
         defense_s_y = s_y,
         defense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver)

# getting defense vs. rusher stats
tot_data1 = defense_run_data %>%
  inner_join(wr_run_data %>% select(-nflId), by = c("gameId", "playId")) %>%
  mutate(defense_receiver_x = defense_x - receiver_x,
         defense_receiver_y = defense_y - receiver_y,
         defense_receiver_s_x = defense_s_x - receiver_s_x,
         defense_receiver_s_y = defense_s_y - receiver_s_y
  ) %>%
  select(-starts_with("receiver"))

# getting defense vs. offense stats
tot_data2 = tot_data1 %>%
  inner_join(offense_run_data, by = c("gameId", "playId")) %>%
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
         defense_offense_s_x, defense_offense_s_y)

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
  inner_join(pass_reception_tot %>%
               distinct(gameId, playId, yards_after_catch)) %>%
  distinct() %>%
  arrange(group, gameId, playId2) %>%
  mutate(yards_after_catch_adj = case_when(yards_after_catch < -10 ~ as.numeric(-10),
                                           yards_after_catch > 50 ~ as.numeric(50),
                                           TRUE ~ as.numeric(yards_after_catch)))


setwd("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/")
write.csv(tot_data4, "yac_tensor_score.csv", row.names = FALSE)
write.csv(target, "yac_target_score.csv", row.names = FALSE)



# Repeating Analysis for INTs ---------------------------------------------

# Reading in The Data -----------------------------------------------------

interceptions = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/interceptions.csv")

# finding plays without dir
plays_remove = interceptions %>%
  filter(is.na(dir)) %>%
  distinct(gameId, playId)

plays_remove2 = interceptions %>%
  group_by(gameId, playId) %>%
  summarize(receivers = sum(IsReceiver)) %>%
  filter(receivers != 1)

interceptions = interceptions %>%
  anti_join(plays_remove) %>%
  anti_join(plays_remove2)

check = interceptions %>%
  distinct(gameId, playId)

# Adding in Extra Plays By Flipping ---------------------------------------
interceptions_tot = interceptions %>%
  mutate(group = "full")

# Deriving Features for Neural Network ------------------------------------

# Features:
# ------------------ FROM KAGGLE WINNERS ------------------------
# def S(x,y)
# def (x,y) - receiver (x,y)
# def S(x,y) - receiver S(x,y)
# off (x,y) - def (x,y)
# off S(x,y) - def S(x,y)
# ---------------------------------------------------------------

# computing S(x,y)
interceptions_tot2 = interceptions_tot %>%
  mutate(s_x = s*cos((90-dir)*pi/180),
         s_y = s*sin((90-dir)*pi/180)) %>%
  select(group, gameId, playId, nflId, x, y, s_x, s_y, IsOnOffense, IsReceiver)

# ball carrier run data
wr_run_data = interceptions_tot2 %>%
  filter(IsReceiver) %>%
  arrange(gameId, playId) %>%
  rename(receiver_x = x,
         receiver_y = y,
         receiver_s_x = s_x,
         receiver_s_y = s_y) %>%
  select(-IsOnOffense, -IsReceiver)

offense_run_data = interceptions_tot2 %>%
  filter(IsOnOffense & (!IsReceiver)) %>%
  arrange(gameId, playId, nflId) %>%
  rename(offense_x = x,
         offense_y = y,
         offense_s_x = s_x,
         offense_s_y = s_y,
         offense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver)

defense_run_data = interceptions_tot2 %>%
  filter(!IsOnOffense) %>%
  arrange(gameId, playId, nflId) %>%
  rename(defense_x = x,
         defense_y = y,
         defense_s_x = s_x,
         defense_s_y = s_y,
         defense_nflId = nflId) %>%
  select(-IsOnOffense, -IsReceiver)

# getting defense vs. rusher stats
tot_data1 = defense_run_data %>%
  inner_join(wr_run_data %>% select(-nflId), by = c("gameId", "playId")) %>%
  mutate(defense_receiver_x = defense_x - receiver_x,
         defense_receiver_y = defense_y - receiver_y,
         defense_receiver_s_x = defense_s_x - receiver_s_x,
         defense_receiver_s_y = defense_s_y - receiver_s_y
  ) %>%
  select(-starts_with("receiver"))

# getting defense vs. offense stats
tot_data2 = tot_data1 %>%
  inner_join(offense_run_data, by = c("gameId", "playId")) %>%
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
         defense_offense_s_x, defense_offense_s_y)

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

max_def_players = 11
max_off_players = 10

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
  inner_join(interceptions %>%
               distinct(gameId, playId, yards_after_catch)) %>%
  distinct() %>%
  arrange(group, gameId, playId2) %>%
  mutate(yards_after_catch_adj = case_when(yards_after_catch < -10 ~ as.numeric(-10),
                                           yards_after_catch > 50 ~ as.numeric(50),
                                           TRUE ~ as.numeric(yards_after_catch)))


setwd("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/")
write.csv(tot_data4, "yaint_tensor.csv", row.names = FALSE)
write.csv(target, "yaint_target.csv", row.names = FALSE)

int_loss = interceptions %>%
  inner_join(interceptions %>%
               filter(yards_after_catch < -5) %>%
               select(gameId, playId) %>%
               filter(row_number() == 15)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

int_loss %>%
  mutate(grouping = case_when(IsReceiver ~ "Interceptor",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(int_loss$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')

int_gain = interceptions %>%
  inner_join(interceptions %>%
               filter(yards_after_catch == 21) %>%
               select(gameId, playId) %>%
               filter(row_number() == 17)) %>%
  mutate(x_end = s*cos((90-dir)*pi/180) + x, 
         y_end = s*sin((90-dir)*pi/180) + y)

int_gain %>%
  mutate(grouping = case_when(IsReceiver ~ "Interceptor",
                              IsOnOffense ~ "Offense",
                              TRUE ~ "Defense")) %>%
  ggplot(aes(x, y, fill = grouping), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = x, y = y, xend = x_end,
                   yend = y_end, colour = grouping), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate",
       title = unique(int_gain$gameId)) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~playId, scale = 'free')
