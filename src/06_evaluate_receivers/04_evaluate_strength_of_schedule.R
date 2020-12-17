# This code scores players tracking ability
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
wr_db_man_matchups = wr_db_man_matchups %>%
  group_by(gameId, playId, nflId_off) %>%
  filter(n() == 1)
wr_db_zone_matchups_tot = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")

epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")

zone_matchups = epa_tracking_total %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  group_by(gameId, playId, nflId, targetNflId, frameId_start) %>%
  mutate(nFrames = max(frameId) - min(frameId) + 1) %>%
  filter(nFrames >= 5) %>%
  dplyr::select(-nFrames) %>%
  distinct(gameId, playId, nflId, targetNflId, frameId_start)
  

pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_attempt) %>%
  rename(comp_prob_pass_attempt = C_prob)

pass_attempt_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_arrived) %>%
  rename(comp_prob_pass_arrived = C_prob)

pass_arrived_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

pass_result_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")

receiver_man_tracking_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_tracking_eps.csv")
receiver_zone_tracking_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_zone_tracking_eps.csv")
receiver_ball_skills_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_ball_skills_eps.csv")
receiver_closing_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_closing_eps.csv")
receiver_tackling_eps = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/receiver_man_tackling_eps.csv")

# Compute Strength of Schedule --------------------------------------------

# man tracking difficulty
man_tracking_sos = wr_db_man_matchups %>%
                    left_join(receiver_man_tracking_eps,
                              by = c("nflId_off" = "targetNflId")) %>%
  dplyr::select(gameId, playId, nflId_def, eps_per_route)

man_tracking_sos[is.na(man_tracking_sos)] = (receiver_man_tracking_eps %>% filter(is.na(targetNflId)))$eps_per_route

man_tracking_sos2 = man_tracking_sos %>%
  group_by(nflId_def) %>%
  summarize(routes = n(),
            man_tracking_expected_eps = sum(eps_per_route),
            man_tracking_sos = mean(eps_per_route)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, routes, everything()) %>%
  arrange(desc(routes))

# zone tracking difficulty
zone_tracking_sos = zone_matchups %>%
  left_join(receiver_zone_tracking_eps,
            by = c("targetNflId" = "targetNflId")) %>%
  dplyr::select(gameId, playId, nflId, eps_per_route)

zone_tracking_sos[is.na(zone_tracking_sos)] = (receiver_zone_tracking_eps %>% filter(is.na(targetNflId)))$eps_per_route

zone_tracking_sos2 = zone_tracking_sos %>%
  group_by(nflId) %>%
  summarize(routes = n(),
            zone_tracking_expected_eps = sum(eps_per_route),
            zone_tracking_sos = mean(eps_per_route)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId" = "nflId")) %>%
  rename(nflId_def = nflId) %>%
  dplyr::select(position, displayName, nflId_def, routes, everything()) %>%
  arrange(desc(routes))

# closing difficulty ------------------------------------------------------
pass_attempt_zone_link = pass_attempt_epa %>%
  inner_join(pass_attempt_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

pass_arrived_zone_link = pass_arrived_epa %>%
  inner_join(pass_arrived_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

wr_db_zone_closing_matchups = pass_attempt_zone_link %>%
  inner_join(pass_arrived_zone_link)

wr_db_man_closing_matchups = pass_attempt_epa %>%
  inner_join(pass_arrived_epa) %>%
  inner_join(targeted_receiver) %>%
  filter(!is.na(targetNflId)) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  rename(nflId_off = targetNflId) %>%
  dplyr::select(names(wr_db_zone_closing_matchups))

tot_closing_matchps = rbind(wr_db_zone_closing_matchups %>%
                              mutate(coverage = "zone"),
                            wr_db_man_closing_matchups %>%
                              mutate(coverage = "man"))

closing_sos = tot_closing_matchps %>%
  left_join(receiver_closing_eps,
            by = c("nflId_off" = "targetNflId")) %>%
    dplyr::select(gameId, playId, nflId_def, coverage, eps_saved_closing_per_target_w_penalties)

closing_sos[is.na(closing_sos)] = (receiver_closing_eps %>% filter(is.na(targetNflId)))$eps_saved_closing_per_target_w_penalties

closing_sos2 = closing_sos %>%
  group_by(coverage, nflId_def) %>%
  summarize(accurate_targets = n(),
            expected_closing_eps = sum(eps_saved_closing_per_target_w_penalties),
            closing_sos = mean(eps_saved_closing_per_target_w_penalties)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(coverage, position, displayName, nflId_def, accurate_targets, everything()) %>%
  arrange(desc(accurate_targets)) %>%
  pivot_wider(names_from = "coverage",
              values_from = c("accurate_targets",
                              "expected_closing_eps",
                              "closing_sos"))

# ball skills

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  dplyr::select(play_id, game_id, interception_player_name, 
                pass_defense_1_player_name,
                pass_defense_2_player_name) %>%
  rename(gameId = game_id,
         playId = play_id)

wr_db_zone_matchups = pass_arrived_epa %>%
  inner_join(pass_arrived_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

interceptions_incompletions_remove = wr_db_zone_matchups %>%
  inner_join(targeted_receiver,
             by = c("gameId", "playId", "nflId_off" = "targetNflId")) %>%
  inner_join(plays %>%
               filter(passResult %in% c("IN", "I")) %>%
               distinct(gameId, playId)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName) %>%
               rename(nflId_def = nflId)) %>%
  left_join(pbp_data_2018) %>%
  separate(displayName, into = c("first", "rest"), sep = "\\s",
           extra = "merge") %>%
  mutate(displayNameNew = paste0(substring(first, 1, 1), ".",
                                 rest)) %>%
  mutate(displayNameNew = case_when(displayNameNew == "S.Griffin" ~ "SL.Griffin",
                                    displayNameNew == "H.Ha Clinton-Dix" ~ "H.Clinton-Dix",
                                    displayNameNew ==  "B.Williams" ~ "Bra.Williams",
                                    displayNameNew == "C.Davis" ~ "C.Davis III",
                                    displayNameNew == "D.Harris" ~ "Dev.Harris",
                                    TRUE ~ displayNameNew)) %>%
  mutate(all_null_flag = is.na(interception_player_name)&
           is.na(pass_defense_1_player_name)&
           is.na(pass_defense_2_player_name)) %>%
  rowwise() %>%
  mutate(is_player = replace_na(grepl(str_to_lower(interception_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(pass_defense_1_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(pass_defense_2_player_name), str_to_lower(displayNameNew)), FALSE),
         interceptor_right = replace_na(grepl(str_to_lower(interception_player_name), str_to_lower(displayNameNew)), TRUE)) 


interceptions_incompletions_remove2 = interceptions_incompletions_remove %>%
  filter(!all_null_flag,
         (!is_player|!interceptor_right))

ball_skills_zone = pass_arrived_epa %>%
  inner_join(pass_result_epa) %>%
  anti_join(interceptions_incompletions_remove2) %>%
  inner_join(targeted_receiver) %>%
  inner_join(plays %>%
               dplyr::select(gameId, playId, passResult)) %>%
  filter(!is.na(targetNflId),
         !is.na(epa_throw)) %>%
  inner_join(wr_db_zone_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  dplyr::select(gameId, playId, targetNflId, nflId_def)


interceptions_incompletions_remove = wr_db_man_matchups %>%
  inner_join(targeted_receiver,
             by = c("gameId", "playId", "nflId_off" = "targetNflId")) %>%
  inner_join(plays %>%
               filter(passResult %in% c("IN", "I")) %>%
               distinct(gameId, playId)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName) %>%
               rename(nflId_def = nflId)) %>%
  left_join(pbp_data_2018) %>%
  separate(displayName, into = c("first", "rest"), sep = "\\s",
           extra = "merge") %>%
  mutate(displayNameNew = paste0(substring(first, 1, 1), ".",
                                 rest)) %>%
  mutate(displayNameNew = case_when(displayNameNew == "S.Griffin" ~ "SL.Griffin",
                                    displayNameNew == "H.Ha Clinton-Dix" ~ "H.Clinton-Dix",
                                    displayNameNew ==  "B.Williams" ~ "Bra.Williams",
                                    displayNameNew == "C.Davis" ~ "C.Davis III",
                                    displayNameNew == "D.Harris" ~ "Dev.Harris",
                                    TRUE ~ displayNameNew)) %>%
  mutate(all_null_flag = is.na(interception_player_name)&
           is.na(pass_defense_1_player_name)&
           is.na(pass_defense_2_player_name)) %>%
  rowwise() %>%
  mutate(is_player = replace_na(grepl(str_to_lower(interception_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(pass_defense_1_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(pass_defense_2_player_name), str_to_lower(displayNameNew)), FALSE),
         interceptor_right = replace_na(grepl(str_to_lower(interception_player_name), str_to_lower(displayNameNew)), TRUE)) 

interceptions_incompletions_remove2 = interceptions_incompletions_remove %>%
  filter(!all_null_flag,
         (!is_player|!interceptor_right))


ball_skills_man = pass_arrived_epa %>%
  inner_join(pass_result_epa) %>%
  anti_join(interceptions_incompletions_remove2) %>%
  inner_join(targeted_receiver) %>%
  inner_join(plays %>%
               dplyr::select(gameId, playId, passResult)) %>%
  filter(!is.na(targetNflId),
         !is.na(epa_throw)) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  dplyr::select(gameId, playId, targetNflId, nflId_def)

tot_ball_skills_matchps = rbind(ball_skills_zone %>%
                                  mutate(coverage = "zone"),
                                ball_skills_man %>%
                                  mutate(coverage = "man"))

ball_skills_sos = tot_ball_skills_matchps %>%
  left_join(receiver_ball_skills_eps,
            by = c("targetNflId" = "targetNflId")) %>%
  dplyr::select(gameId, playId, nflId_def, coverage, eps_saved_ball_skills_per_target_w_penalties)

ball_skills_sos[is.na(ball_skills_sos)] = (receiver_ball_skills_eps %>% filter(is.na(targetNflId)))$eps_saved_ball_skills_per_target_w_penalties

ball_skills_sos2 = ball_skills_sos %>%
  group_by(coverage, nflId_def) %>%
  summarize(accurate_targets = n(),
            expected_ball_skills_eps = sum(eps_saved_ball_skills_per_target_w_penalties),
            ball_skills_sos = mean(eps_saved_ball_skills_per_target_w_penalties)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, accurate_targets, everything()) %>%
  arrange(desc(accurate_targets)) %>%
  pivot_wider(names_from = "coverage",
              values_from = c("accurate_targets",
                              "expected_ball_skills_eps",
                              "ball_skills_sos"))

# tackling

setwd("~/Desktop/NFL_PBP_DATA/")
pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
  dplyr::select(play_id, game_id, forced_fumble_player_1_player_name,
                solo_tackle_1_player_name,
                assist_tackle_1_player_name, assist_tackle_2_player_name,
                assist_tackle_3_player_name, assist_tackle_4_player_name) %>%
  rename(gameId = game_id,
         playId = play_id)

# Identifying Fumbles Forced to Remove -------------------------------------

fumbles_forced_remove = wr_db_man_matchups %>%
  inner_join(targeted_receiver,
             by = c("gameId", "playId", "nflId_off" = "targetNflId")) %>%
  inner_join(plays %>%
               filter(passResult %in% c("C"),
                      grepl("fumble", str_to_lower(playDescription)),
                      penaltyCodes == "") %>%
               distinct(gameId, playId)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName) %>%
               rename(nflId_def = nflId)) %>%
  left_join(pbp_data_2018) %>%
  separate(displayName, into = c("first", "rest"), sep = "\\s",
           extra = "merge") %>%
  mutate(displayNameNew = paste0(substring(first, 1, 1), ".",
                                 rest)) %>%
  mutate(displayNameNew = case_when(displayNameNew == "S.Griffin" ~ "SL.Griffin",
                                    displayNameNew == "H.Ha Clinton-Dix" ~ "H.Clinton-Dix",
                                    displayNameNew ==  "B.Williams" ~ "Bra.Williams",
                                    displayNameNew == "C.Davis" ~ "C.Davis III",
                                    displayNameNew == "D.Harris" ~ "Dev.Harris",
                                    TRUE ~ displayNameNew)) %>%
  rowwise() %>%
  mutate(is_player = replace_na(grepl(str_to_lower(forced_fumble_player_1_player_name), str_to_lower(displayNameNew)), FALSE))


fumbles_forced_remove2 = fumbles_forced_remove %>%
  filter(!is_player)

plays_no_penalties = plays %>%
  filter(penaltyCodes == "")


tackling_ability_man = pass_result_epa %>%
  filter(!is.na(epa_yac)) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  inner_join(plays_no_penalties %>%
               distinct(gameId, playId)) %>%
  anti_join(fumbles_forced_remove2) %>%
  dplyr::select(gameId, playId, targetNflId, nflId_def)

pass_result_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/pass_attempts_all.csv") %>%
  distinct(gameId, playId, frameId)

wr_db_zone_matchups = pass_result_epa %>%
  inner_join(pass_result_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

fumbles_forced_remove = wr_db_zone_matchups %>%
  inner_join(targeted_receiver,
             by = c("gameId", "playId", "nflId_off" = "targetNflId")) %>%
  inner_join(plays %>%
               filter(passResult %in% c("C"),
                      grepl("fumble", str_to_lower(playDescription)),
                      penaltyCodes == "") %>%
               distinct(gameId, playId)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName) %>%
               rename(nflId_def = nflId)) %>%
  left_join(pbp_data_2018) %>%
  separate(displayName, into = c("first", "rest"), sep = "\\s",
           extra = "merge") %>%
  mutate(displayNameNew = paste0(substring(first, 1, 1), ".",
                                 rest)) %>%
  mutate(displayNameNew = case_when(displayNameNew == "S.Griffin" ~ "SL.Griffin",
                                    displayNameNew == "H.Ha Clinton-Dix" ~ "H.Clinton-Dix",
                                    displayNameNew ==  "B.Williams" ~ "Bra.Williams",
                                    displayNameNew == "C.Davis" ~ "C.Davis III",
                                    displayNameNew == "D.Harris" ~ "Dev.Harris",
                                    TRUE ~ displayNameNew)) %>%
  rowwise() %>%
  mutate(is_player = replace_na(grepl(str_to_lower(forced_fumble_player_1_player_name), str_to_lower(displayNameNew)), FALSE))


fumbles_forced_remove2 = fumbles_forced_remove %>%
  filter(!is_player)

plays_no_penalties = plays %>%
  filter(penaltyCodes == "")


# Scoring All Plays -------------------------------------------------------

tackling_ability_zone  = pass_result_epa %>%
  filter(!is.na(epa_yac)) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_zone_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  inner_join(plays_no_penalties %>%
               distinct(gameId, playId)) %>%
  anti_join(fumbles_forced_remove2) %>%
  dplyr::select(gameId, playId, targetNflId, nflId_def)

tot_tackling_matchups = rbind(tackling_ability_zone %>%
                                  mutate(coverage = "zone"),
                                tackling_ability_man %>%
                                  mutate(coverage = "man"))

tackling_sos = tot_tackling_matchups %>%
  left_join(receiver_tackling_eps,
            by = c("targetNflId" = "targetNflId")) %>%
  dplyr::select(gameId, playId, nflId_def, coverage, eps_per_tackling_opportunity)

tackling_sos[is.na(tackling_sos)] = (receiver_tackling_eps %>% filter(is.na(targetNflId)))$eps_per_tackling_opportunity

tackling_sos2 = tackling_sos %>%
  group_by(coverage, nflId_def) %>%
  summarize(tackling_opportunties = n(),
            expected_tackling_eps = sum(eps_per_tackling_opportunity),
            ball_tackling_sos = mean(eps_per_tackling_opportunity)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, tackling_opportunties, everything()) %>%
  arrange(desc(tackling_opportunties)) %>%
  pivot_wider(names_from = "coverage",
              values_from = c("tackling_opportunties",
                              "expected_tackling_eps",
                              "ball_tackling_sos"))


# summarizing sos metrics -------------------------------------------------

sos_tot = zone_tracking_sos2 %>%
  dplyr::select(position, displayName, nflId_def, zone_tracking_expected_eps,
         zone_tracking_sos) %>%
  full_join(man_tracking_sos2 %>%
              dplyr::select(position, displayName, nflId_def, man_tracking_expected_eps,
                     man_tracking_sos)) %>%
  full_join(closing_sos2 %>%
              dplyr::select(position, displayName, nflId_def, expected_closing_eps_zone,
                            closing_sos_zone, expected_closing_eps_man, closing_sos_man)) %>%
  full_join(ball_skills_sos2 %>%
              dplyr::select(position, displayName, nflId_def, expected_ball_skills_eps_zone,
                            ball_skills_sos_zone, expected_ball_skills_eps_man, ball_skills_sos_man)) %>%
  full_join(tackling_sos2 %>%
              dplyr::select(position, displayName, nflId_def, expected_tackling_eps_zone,
                            ball_tackling_sos_man, expected_tackling_eps_man, ball_tackling_sos_man))
  
sos_tot[is.na(sos_tot)] = 0

sos_tot2 = sos_tot %>%
  mutate(expected_eps_zone = zone_tracking_expected_eps + expected_closing_eps_zone +
           expected_ball_skills_eps_zone + expected_tackling_eps_zone,
         expected_eps_man = man_tracking_expected_eps + expected_closing_eps_man +
           expected_ball_skills_eps_man + expected_tackling_eps_man,
         expected_eps_tot = expected_eps_zone + expected_eps_man) %>%
  dplyr::select(position, displayName, nflId_def, expected_eps_tot, expected_eps_zone,
                expected_eps_man, everything()) %>%
  arrange(expected_eps_tot)


# Adjusting Rankings ------------------------------------------------------

zone_ratings = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/overall_player_skills_summary.csv")
man_ratings = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players//outputs/overall_player_skills_summary.csv")

tot_ratings = zone_ratings %>%
  full_join(man_ratings,
            by = c("position", "displayName", "nflId_def")) %>%
  rename(eps_tracking_zone = eps_tracking_w_penalties.x,
         eps_tracking_man = eps_tracking_w_penalties.y,
         eps_closing_zone = eps_saved_closing_w_penalties.x,
         eps_closing_man = eps_saved_closing_w_penalties.y,
         eps_ball_skills_zone = eps_saved_ball_skills_w_penalties.x,
         eps_ball_skills_man = eps_saved_ball_skills_w_penalties.y,
         eps_tackling_zone = eps_tackling.x,
         eps_tackling_man = eps_tackling.y) %>%
  mutate(eps_tot = eps_zone_coverage + eps_man_coverage) %>%
  dplyr::select("position", "displayName", "nflId_def",
                "eps_tot", "eps_zone_coverage","eps_man_coverage",
                "eps_tracking_zone", "eps_tracking_man",
                "eps_closing_zone", "eps_closing_man",
                "eps_ball_skills_zone","eps_ball_skills_man",
                "eps_tackling_zone", "eps_tackling_man") %>%
  arrange(desc(eps_tot))

tot_ratings[is.na(tot_ratings)] = 0

tot_ratings2 = tot_ratings %>%
  inner_join(sos_tot2) %>%
  mutate(eps_tot_sos_adj = eps_tot - expected_eps_tot,
         eps_zone_sos_adj = eps_zone_coverage - expected_eps_zone,
         eps_man_sos_adj = eps_man_coverage - expected_eps_man) %>%
  dplyr::select("position", "displayName", "nflId_def",
                "eps_tot", "eps_zone_coverage","eps_man_coverage",
                "eps_tot_sos_adj", "eps_zone_sos_adj", "eps_man_sos_adj",
                everything())
  

