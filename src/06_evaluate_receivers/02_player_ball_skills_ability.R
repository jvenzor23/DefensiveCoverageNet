# This code scores players ball skills ability
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
# 4) For interceptions, we need to ensure it was the player in man coverage
#    (not another player coming in to help in a zone) that actually made the
#    interception!

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
drops = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/drops.csv")

wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
# dropping WRs with double teams
wr_db_man_matchups = wr_db_man_matchups %>%
  group_by(gameId, playId, nflId_off) %>%
  filter(n() == 1)

pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, IN_prob, epa_pass_arrived) %>%
  rename(comp_prob_pass_arrived = C_prob,
         int_prob_pass_arrived = IN_prob)

pass_result_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv") %>%
  dplyr::select(gameId, playId, epa_throw)

dpi_classification = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/pass_interference_classification/outputs/dpi_classification.csv")

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

player_numbers = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/player_numbers.csv")

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
    dplyr::select(play_id, game_id, interception_player_name, 
                  pass_defense_1_player_name,
                  pass_defense_2_player_name) %>%
    rename(gameId = game_id,
           playId = play_id)


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

ball_skills_defensive_penalties_score = closing_defensive_penalties_remove %>%
  inner_join(targeted_receiver) %>%
  filter(penaltyCodes == "DPI",
        (enforced_flag & !declined_flag & !offsetting_flag & (ball_skills_dpi == 1))) %>%
  dplyr::select(gameId, playId, targetNflId, penaltyCodes, enforced_flag, declined_flag, offsetting_flag, my_epa)

ball_skills_defensive_penalties_man_avg = (ball_skills_defensive_penalties_score %>%
    filter(penaltyCodes == "DPI") %>%
    ungroup() %>%
    summarize(avg_penalty_epa_per_target = mean(my_epa)))$avg_penalty_epa_per_target*dim(ball_skills_defensive_penalties_score)[1]/
  (dim(ball_skills_defensive_penalties_score)[1] + dim(pass_arrived_epa %>%
                                                         inner_join(targeted_receiver) %>%
                                                         inner_join(wr_db_man_matchups,
                                                                   by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
                                                                      distinct(gameId, playId))[1])

# Joining the Data --------------------------------------------------------

ball_skills_ability = pass_arrived_epa %>%
  inner_join(pass_result_epa) %>%
  inner_join(targeted_receiver) %>%
  inner_join(plays %>%
               dplyr::select(gameId, playId, passResult)) %>%
  filter(!is.na(targetNflId),
         !is.na(epa_throw)) %>%
  anti_join(drops)

ball_skills_ability$fitted_epa_pass_throw = lm(epa_throw ~ epa_pass_arrived, data = ball_skills_ability)$fitted.values

summary(lm(epa_throw ~ epa_pass_arrived, data = ball_skills_ability))

ball_skills_ability %>%
  ggplot() +
  geom_point(aes(x = epa_pass_arrived, y = epa_throw)) +
  geom_line(aes(x = epa_pass_arrived, y = fitted_epa_pass_throw), color = "blue")


# Grouping By Player ------------------------------------------------------

ball_skills_ability2 = ball_skills_ability %>%
  group_by(targetNflId) %>%
  summarize(targets = n(),
            completions = sum(passResult == 'C'),
            interceptions = sum(passResult == 'IN'),
            completion_perc = mean(passResult == 'C'),
            interception_perc = mean(passResult == 'IN'),
            e_comp_perc = mean(comp_prob_pass_arrived),
            e_int_perc = mean(int_prob_pass_arrived),
            completions_saved = sum(comp_prob_pass_arrived) - sum(passResult == 'C'),
            completions_saved_per_target = completions_saved/targets,
            interceptions_created =  sum(passResult == 'IN') - sum(int_prob_pass_arrived),
            interceptions_created_per_target = completions_saved/targets,
            expected_epa = sum(epa_pass_arrived),
            actual_epa = sum(epa_throw),
            epa_pass_arrived_avg = mean(epa_pass_arrived),
            epa_result_avg = mean(epa_throw),
            eps_saved_ball_skills = expected_epa - actual_epa,
            eps_saved_ball_skills_per_target = eps_saved_ball_skills/targets) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("targetNflId" = "nflId")) %>%
  dplyr::select(position, displayName, targetNflId, targets, everything()) %>%
  mutate(qualifying = if_else(targets >= 30, 1, 0)) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), desc(eps_saved_ball_skills))

ball_skills_ability2 %>%
  ggplot() +
  geom_point(aes(x = epa_pass_arrived_avg, y = eps_saved_ball_skills)) +
  geom_smooth(aes(x = epa_pass_arrived_avg, y = eps_saved_ball_skills), method = 'lm')


# Adding In Penalties -----------------------------------------------------

ball_skills_penalty = ball_skills_defensive_penalties_score %>%
  mutate(epa_penalty = my_epa - mean(pass_arrived_epa$epa_pass_arrived)) %>%
  group_by(targetNflId) %>%
  summarize(ball_skills_penalties = n(),
            avg_ball_skills_penalties_eps = -1*mean(epa_penalty))


ball_skills_ability3 = ball_skills_ability2 %>%
  left_join(ball_skills_penalty)

ball_skills_ability3[is.na(ball_skills_ability3)] = 0  

ball_skills_ability3 = ball_skills_ability3 %>%
  mutate(eps_saved_ball_skills_per_target_w_penalties = (eps_saved_ball_skills_per_target*targets + 
                                                      ball_skills_penalties*avg_ball_skills_penalties_eps)/(ball_skills_penalties + targets),
         eps_saved_ball_skills_w_penalties = eps_saved_ball_skills + ball_skills_penalties*avg_ball_skills_penalties_eps + 
           ball_skills_defensive_penalties_man_avg*(ball_skills_penalties + targets)) %>%
  arrange(desc(qualifying), desc(eps_saved_ball_skills_per_target_w_penalties)) %>%
  dplyr::select(qualifying, position, displayName, 
                targetNflId, targets, eps_saved_ball_skills_per_target_w_penalties)

ball_skills_ability4 = rbind(
  ball_skills_ability3 %>%
    filter(qualifying == 1),
  ball_skills_ability3 %>%
    filter(qualifying == 0) %>%
    ungroup() %>%
    summarize(eps_saved_ball_skills_per_target_w_penalties = sum(targets*eps_saved_ball_skills_per_target_w_penalties)/sum(targets),
              targets = sum(targets)) %>%
    mutate(qualifying = 0,
           nflId = NA,
           targetNflId = NA, 
           displayName = NA,
           position = NA) %>%
    dplyr::select(qualifying, position, displayName, 
                  targetNflId, targets, eps_saved_ball_skills_per_target_w_penalties)
)

write.csv(ball_skills_ability4,
          "~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_ball_skills_eps.csv",
          row.names = FALSE)
