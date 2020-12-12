# This code scores players closing ability
# RULES:
# 1) all plays where a player commits a penalty during the tracking phase are excluded
#    from scoring based on our epa tracking values, and will be assinged the negative
#    epa value associated with the penalty
# -- this includes defensive holding, illegal contact, and illegal use of hands
# 2) any accepted penalty will be held against a player, while any declined
#    or offsetting penalty will not be held against a player (but that player
#    will not be evaluated based on their tracking with epa values for that
#    play!)
# 3) Unfortunately, plays with accepted penalties do not include routes.
#    As a result, the valid data on all non-penalized players are not
#    included in this analysis. To fix, we would update the 00_get_tracking_pt_data.R
#    function called in 00_score_plays.R to include these players (although
#    we still would not know their route without doing much more work to build
#    a classification algorithm based on assigned routes)

# Clean workspace
rm(list=ls())

# Setting Working Directory
setwd("~/Desktop/CoverageNet/inputs/")

# Calling Necessary Libraries
library(tidyverse)
# library(dplyr)
library(ggplot2)
library(lubridate)
library(reticulate)
library(gganimate)
library(magick)
library(fitdistrplus)
library(skewt)
library(sn)
library(broom)
library(dplyr)


# Reading in The Data -----------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")

wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
# dropping WRs with double teams
wr_db_man_matchups = wr_db_man_matchups %>%
  group_by(gameId, playId, nflId_off) %>%
  filter(n() == 1)

pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_attempt) %>%
  rename(comp_prob_pass_attempt = C_prob)

pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_arrived) %>%
  rename(comp_prob_pass_arrived = C_prob)

dpi_classification = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/pass_interference_classification/outputs/dpi_classification.csv")

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

player_numbers = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/player_numbers.csv")

# Identifying Penalties to Remove -----------------------------------------

# offsetting penalties/decline penalties:
# we cannot identify which period these are in, so they will be removed!

plays_penalties_remove1 = plays %>%
  filter(penaltyCodes != "") %>%
  dplyr::select(gameId, playId, playDescription, possessionTeam, penaltyCodes, penaltyJerseyNumbers,
                offensePlayResult, playResult, isDefensivePI) %>%
  inner_join(my_epa) %>%
  rowwise() %>%
  mutate(penalty_desc = trimws(str_split(str_split(str_to_lower(playDescription), "penalty")[[1]][2], ",")[[1]][2])) %>%
  mutate(enforced_flag = grepl("enforced", str_to_lower(playDescription)),
         declined_flag = grepl("declined", str_to_lower(playDescription)),
         offsetting_flag = grepl("offsetting", str_to_lower(playDescription))) %>%
  left_join(targeted_receiver) %>%
  left_join(dpi_classification) 

plays_penalties_remove2 = plays_penalties_remove1 %>%
  separate_rows(penaltyCodes, penaltyJerseyNumbers, 
                sep = ";", convert = TRUE) %>%
  dplyr::select(gameId, playId, possessionTeam, penaltyCodes, penaltyJerseyNumbers,
                ends_with("flag"), ends_with("dpi"), isDefensivePI, my_epa) %>%
  rowwise() %>%
  mutate(isDefensivePenalty = (str_to_lower(possessionTeam) !=
                                 str_split(str_to_lower(penaltyJerseyNumbers),
                                           " ")[[1]][1]))


closing_defensive_penalties_remove = plays_penalties_remove2 %>%
  filter(penaltyCodes %in% c('DPI', 'OPI')) %>%
  mutate(jerseyNumber = str_split(penaltyJerseyNumbers, " ")[[1]][2]) %>%
  dplyr::select(gameId, playId, possessionTeam, penaltyCodes, jerseyNumber,
                ends_with("flag"), ends_with("dpi"), isDefensivePI, isDefensivePenalty, my_epa)

closing_defensive_penalties_score = closing_defensive_penalties_remove %>%
  inner_join(player_numbers %>%
               mutate(jerseyNumber = as.character(jerseyNumber)),
             by = c("gameId","jerseyNumber","possessionTeam",
                    "isDefensivePenalty" = "isOnDefense")) %>%
  inner_join(targeted_receiver) %>%
  filter(penaltyCodes == "DPI",
        (enforced_flag & !declined_flag & !offsetting_flag & (ball_skills_dpi == 0))) %>%
  dplyr::select(gameId, playId, targetNflId, penaltyCodes, enforced_flag, declined_flag, offsetting_flag, my_epa)


closing_defensive_penalties_man_avg = pass_attempt_epa %>%
  inner_join(targeted_receiver) %>%
  left_join(closing_defensive_penalties_remove %>%
              filter(penaltyCodes == "DPI")) %>%
  ungroup() %>%
  summarize(avg_penalty_epa_per_target = mean(replace_na(my_epa, 0)))

# Joining the Data --------------------------------------------------------

closing_ability = pass_attempt_epa %>%
  inner_join(pass_arrived_epa) %>%
  inner_join(targeted_receiver) %>%
  filter(!is.na(targetNflId))

closing_ability$fitted_epa_pass_arrived = lm(epa_pass_arrived ~ epa_pass_attempt, data = closing_ability)$fitted.values
closing_ability$fitted_comp_prob_pass_arrived = lm(comp_prob_pass_arrived ~ comp_prob_pass_attempt, data = closing_ability)$fitted.values

summary(lm(epa_pass_arrived ~ epa_pass_attempt, data = closing_ability))
summary(lm(comp_prob_pass_arrived ~ comp_prob_pass_attempt, data = closing_ability))

closing_ability %>%
  ggplot() +
  geom_point(aes(x = epa_pass_attempt, y = epa_pass_arrived)) +
  geom_line(aes(x = epa_pass_attempt, y = fitted_epa_pass_arrived), color = "blue")

closing_ability %>%
  ggplot() +
  geom_point(aes(x = comp_prob_pass_attempt, y = comp_prob_pass_arrived)) +
  geom_line(aes(x = comp_prob_pass_attempt, y = fitted_comp_prob_pass_arrived), color = "blue")


# Grouping By Player ------------------------------------------------------

closing_ability2 = closing_ability %>%
  group_by(targetNflId) %>%
  summarize(targets = n(),
            expected_epa = sum(fitted_epa_pass_arrived),
            actual_epa = sum(epa_pass_arrived),
            epa_pass_attempt_avg = mean(epa_pass_attempt),
            eps_saved_closing = expected_epa - actual_epa,
            eps_saved_closing_per_target = eps_saved_closing/targets) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("targetNflId" = "nflId")) %>%
  dplyr::select(position, displayName, targetNflId, targets, everything()) %>%
  arrange(desc(eps_saved_closing))

closing_ability2 %>%
  ggplot() +
  geom_point(aes(x = epa_pass_attempt_avg, y = eps_saved_closing_per_target)) +
  geom_smooth(aes(x = epa_pass_attempt_avg, y = eps_saved_closing_per_target), method = 'lm')


# Adding In Penalties -----------------------------------------------------

closing_ability_penalty = pass_attempt_epa %>%
  inner_join(closing_defensive_penalties_score) %>%
  inner_join(my_epa) %>%
  mutate(fitted_epa_pass_arrived = lm(epa_pass_arrived ~ epa_pass_attempt, data = closing_ability)$coef[1]
                      + lm(epa_pass_arrived ~ epa_pass_attempt, data = closing_ability)$coef[2]*epa_pass_attempt) %>%
  mutate(epa_penalty = my_epa - fitted_epa_pass_arrived) %>%
  group_by(targetNflId) %>%
  summarize(closing_penalties = n(),
            avg_closing_penalties_eps = -1*mean(epa_penalty))


closing_ability3 = closing_ability2 %>%
  left_join(closing_ability_penalty)

closing_ability3[is.na(closing_ability3)] = 0  

closing_ability3 = closing_ability3 %>%
  mutate(eps_saved_closing_per_target_w_penalties = (eps_saved_closing_per_target*targets + 
           closing_penalties*avg_closing_penalties_eps)/(closing_penalties + targets),
         eps_saved_closing_w_penalties = eps_saved_closing + closing_penalties*avg_closing_penalties_eps +
           (closing_defensive_penalties_man_avg$avg_penalty_epa_per_target)*(targets + closing_penalties)) %>%
  dplyr::select(position, displayName, targetNflId, targets,
                eps_saved_closing_per_target_w_penalties, eps_saved_closing_w_penalties) %>%
  mutate(qualifying = if_else(targets >= 30, 1, 0)) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), desc(eps_saved_closing_per_target_w_penalties)) %>%
  dplyr::select(qualifying, position, displayName, 
                targetNflId, targets, eps_saved_closing_per_target_w_penalties)

closing_ability4 = rbind(
  closing_ability3 %>%
    filter(qualifying == 1),
  closing_ability3 %>%
    filter(qualifying == 0) %>%
    ungroup() %>%
    summarize(eps_saved_closing_per_target_w_penalties = sum(targets*eps_saved_closing_per_target_w_penalties)/sum(targets),
              targets = sum(targets)) %>%
    mutate(qualifying = 0,
           nflId = NA,
           targetNflId = NA, 
           displayName = NA,
           position = NA) %>%
    dplyr::select(qualifying, position, displayName, 
                  targetNflId, targets, eps_saved_closing_per_target_w_penalties)
)

write.csv(closing_ability4,
          "~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_closing_eps.csv",
          row.names = FALSE)

check = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tracking_eps.csv")
