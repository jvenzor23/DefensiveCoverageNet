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
drops = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/drops.csv")
routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
wr_db_zone_matchups_tot = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")

pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_attempt) %>%
  rename(comp_prob_pass_attempt = C_prob)

# pass arrived data
pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, IN_prob, epa_pass_arrived) %>%
  rename(comp_prob_pass_arrived = C_prob,
         int_prob_pass_arrived = IN_prob)

pass_arrived_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

# pass result data
pass_result_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv") %>%
  dplyr::select(gameId, playId, epa_throw, epa_yaint)

pass_result_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/pass_attempts_all.csv") %>%
  distinct(gameId, playId, frameId)

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

# Creating WR/DB zone links across time frames ------------------------------

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

# Identifying Interceptions to Remove -------------------------------------

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
  mutate(is_player = replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE)|
           replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", pass_defense_1_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE)|
           replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", pass_defense_2_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE),
         interceptor_right = replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), TRUE)) 

distinct_nflScrapR_names = interceptions_incompletions_remove %>%
  dplyr::select(ends_with("player_name")) %>%
  pivot_longer(cols = ends_with("player_name"),
               values_to = "nflScrapeR_names") %>%
  distinct(nflScrapeR_names) %>%
  filter(!is.na(nflScrapeR_names)) %>%
  arrange(nflScrapeR_names)

distinct_BDP_names = interceptions_incompletions_remove %>%
  group_by(displayNameNew) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

distinct_unmatched_BDB_names = distinct_BDP_names %>%
  anti_join(distinct_nflScrapR_names, 
            by = c("displayNameNew" = "nflScrapeR_names"))

interceptions_incompletions_remove2 = interceptions_incompletions_remove %>%
  filter(!all_null_flag,
         (!is_player|!interceptor_right))

pbus = interceptions_incompletions_remove %>%
  filter(is_player & is.na(interception_player_name)) %>%
  group_by(nflId_def) %>%
  summarize(PB = n())

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

# for average defensive penalty info, we need to have pulled the frameId for 
# ball arrival on plays with PI. This will be fixed once we fix the scoring of
# pass arrival to include plays with PI!
ball_skills_defensive_penalties_score = closing_defensive_penalties_remove %>%
  inner_join(player_numbers %>%
               mutate(jerseyNumber = as.character(jerseyNumber)),
             by = c("gameId","jerseyNumber","possessionTeam",
                    "isDefensivePenalty" = "isOnDefense")) %>%
  anti_join(wr_db_man_matchups,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  filter(penaltyCodes == "DPI",
        (enforced_flag & !declined_flag & !offsetting_flag & (ball_skills_dpi == 1))) %>%
  dplyr::select(gameId, playId, nflId, penaltyCodes, enforced_flag, declined_flag, offsetting_flag, my_epa) %>%
  rename(nflId_def = nflId)

ball_skills_defensive_penalties_man_avg = (ball_skills_defensive_penalties_score %>%
    filter(penaltyCodes == "DPI") %>%
    ungroup() %>%
    summarize(avg_penalty_epa_per_target = mean(my_epa)))$avg_penalty_epa_per_target*dim(ball_skills_defensive_penalties_score)[1]/
  (dim(ball_skills_defensive_penalties_score)[1] + dim(pass_arrived_epa %>%
                                                         inner_join(targeted_receiver) %>%
                                                         inner_join(wr_db_zone_matchups,
                                                                   by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
                                                                      distinct(gameId, playId))[1])

# Joining the Data --------------------------------------------------------

ball_skills_ability = pass_arrived_epa %>%
  inner_join(pass_result_epa) %>%
  anti_join(interceptions_incompletions_remove2) %>%
  inner_join(targeted_receiver) %>%
  inner_join(plays %>%
               dplyr::select(gameId, playId, passResult)) %>%
  filter(!is.na(targetNflId),
         !is.na(epa_throw)) %>%
  inner_join(wr_db_zone_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  anti_join(drops)

ball_skills_ability$fitted_epa_pass_throw = lm(epa_throw ~ epa_pass_arrived, data = ball_skills_ability)$fitted.values

summary(lm(epa_throw ~ epa_pass_arrived, data = ball_skills_ability))

ball_skills_ability %>%
  ggplot() +
  geom_point(aes(x = epa_pass_arrived, y = epa_throw)) +
  geom_line(aes(x = epa_pass_arrived, y = fitted_epa_pass_throw), color = "blue")

player_extremes_ball_skill_ability = ball_skills_ability %>%
  mutate(eps_ball_skills = round(epa_pass_arrived - epa_throw, 3)) %>%
  dplyr::select(gameId, playId, nflId_def, targetNflId, eps_ball_skills) %>%
  arrange(nflId_def, desc(eps_ball_skills))

# Identifying Miscellaneous pbus/interceptions! ---------------------------

# getting all zone players
man_zone_classification = rbind(
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_pass_attempts_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_sacks_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability)
) %>%
  arrange(gameId, playId, nflId) %>%
  distinct(gameId, playId, nflId, .keep_all = TRUE)

man_coverage = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")

zone_coverage_players = man_zone_classification %>%
  anti_join(man_coverage,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  distinct(nflId)

players_clean = players %>%
  inner_join(zone_coverage_players) %>%
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
  mutate(displayNameNew_clean = str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))) %>%
  dplyr::select(nflId, displayNameNew_clean)

zone_coverage_plays = man_zone_classification %>%
  anti_join(man_coverage,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  distinct(gameId, playId, nflId)

zone_coverage_plays_not_accounted_for = zone_coverage_plays %>%
  anti_join(ball_skills_ability,
              by = c("gameId", "playId", "nflId" = "nflId_def"))

interceptions_incompletions_not_identified = zone_coverage_plays_not_accounted_for %>%
  inner_join(plays %>%
               filter(passResult %in% c("IN", "I")) %>%
               distinct(gameId, playId)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName)) %>%
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
  mutate(is_player = replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE)|
           replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", pass_defense_1_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE)|
           replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", pass_defense_2_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE),
         interceptor_right = replace_na(grepl(str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name))), str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", displayNameNew)))), FALSE)) %>%
  filter(!all_null_flag,
         (is_player|interceptor_right)) %>%
  filter(is.na(interception_player_name)|interceptor_right)

ball_hawks_int = interceptions_incompletions_not_identified %>%
  dplyr::select(gameId, playId, interception_player_name) %>%
  filter(!is.na(interception_player_name))  %>%
  mutate(interception_player_name_clean = str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name)))) %>%
  inner_join(players_clean,
             by = c("interception_player_name_clean"="displayNameNew_clean")) %>%
  inner_join(zone_coverage_plays) %>%
  inner_join(pass_attempt_epa) %>%
  inner_join(pass_result_epa) %>%
  filter(!is.na(epa_throw)) %>%
  anti_join(drops) %>%
  group_by(nflId) %>%
  summarize(ball_hawk_ints = n(),
            ball_hawk_ints_eps = sum(epa_pass_attempt - epa_throw),
            ball_hawk_eps_int_returns = -1*sum(epa_yaint))

ball_hawks_pbus = interceptions_incompletions_not_identified %>%
  dplyr::select(gameId, playId, interception_player_name, pass_defense_1_player_name) %>%
  filter(is.na(interception_player_name)) %>%
  mutate(pass_defense_1_player_name_clean = str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", pass_defense_1_player_name)))) %>%
  inner_join(players_clean,
             by = c("pass_defense_1_player_name_clean"="displayNameNew_clean")) %>%
  inner_join(zone_coverage_plays) %>%
  inner_join(pass_attempt_epa) %>%
  inner_join(pass_result_epa) %>%
  filter(!is.na(epa_throw)) %>%
  group_by(nflId) %>%
  summarize(ball_hawk_pbus = n(),
            ball_hawk_pbus_eps = sum(epa_pass_attempt - epa_throw))

ball_hawk_stats = ball_hawks_int %>%
  full_join(ball_hawks_pbus)

ball_hawk_stats[is.na(ball_hawk_stats)] = 0

player_extremes_ball_hawk =  rbind(interceptions_incompletions_not_identified %>%
                                     dplyr::select(gameId, playId, interception_player_name) %>%
                                     filter(!is.na(interception_player_name))  %>%
                                     mutate(interception_player_name_clean = str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name)))) %>%
                                     inner_join(players_clean,
                                                by = c("interception_player_name_clean"="displayNameNew_clean")) %>%
                                     inner_join(zone_coverage_plays) %>%
                                     inner_join(pass_attempt_epa) %>%
                                     inner_join(pass_result_epa) %>%
                                     filter(!is.na(epa_throw)) %>%
                                     dplyr::select(-ends_with("name"), -ends_with("clean")) %>%
                                     mutate(play_type = "INT"),
                                   interceptions_incompletions_not_identified %>%
                                     dplyr::select(gameId, playId, interception_player_name, pass_defense_1_player_name) %>%
                                     filter(is.na(interception_player_name)) %>%
                                     mutate(pass_defense_1_player_name_clean = str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", pass_defense_1_player_name)))) %>%
                                     inner_join(players_clean,
                                                by = c("pass_defense_1_player_name_clean"="displayNameNew_clean")) %>%
                                     inner_join(zone_coverage_plays) %>%
                                     inner_join(pass_attempt_epa) %>%
                                     inner_join(pass_result_epa) %>%
                                     filter(!is.na(epa_throw)) %>%
                                     dplyr::select(-ends_with("name"), -ends_with("clean")) %>%
                                     mutate(play_type = "PB")) %>%
  ungroup() %>%
  mutate(eps_ball_hawk = round(epa_pass_attempt - epa_throw, 3)) %>%
  dplyr::select(gameId, playId, nflId, play_type, eps_ball_hawk) %>%
  rename(nflId_def = nflId) %>%
  arrange(nflId_def, desc(eps_ball_hawk))

player_extremes_int_returns = rbind(ball_skills_ability %>%
                                      filter(!is.na(epa_yaint)) %>%
                                      dplyr::select(gameId, playId, nflId_def, epa_yaint),
                                    interceptions_incompletions_not_identified %>%
                                      dplyr::select(gameId, playId, interception_player_name) %>%
                                      filter(!is.na(interception_player_name))  %>%
                                      mutate(interception_player_name_clean = str_to_lower(trimws(gsub("(III)|(II)|(Jr.)|(Sr.)", "", interception_player_name)))) %>%
                                      inner_join(players_clean,
                                                 by = c("interception_player_name_clean"="displayNameNew_clean")) %>%
                                      inner_join(zone_coverage_plays) %>%
                                      inner_join(pass_attempt_epa) %>%
                                      inner_join(pass_result_epa) %>%
                                      filter(!is.na(epa_throw)) %>%
                                      dplyr::select(gameId, playId, nflId, epa_yaint) %>%
                                      rename(nflId_def = nflId)) %>%
  mutate(eps_yaint = -1*epa_yaint) %>%
  dplyr::select(-epa_yaint) %>%
  arrange(nflId_def, desc(eps_yaint))

avg_ball_hawk_per_route = (sum(ball_hawk_stats$ball_hawk_ints_eps) + 
                             sum(ball_hawk_stats$ball_hawk_pbus_eps))/
  dim(zone_coverage_plays)[1]

expected_ball_hawk_eps = zone_coverage_plays %>%
  rename(nflId_def = nflId) %>%
  group_by(nflId_def) %>%
  summarize(routes = n()) %>%
  mutate(e_ball_hawk_eps = routes*avg_ball_hawk_per_route)


# Grouping By Player ------------------------------------------------------

ball_skills_ability2 = ball_skills_ability %>%
  group_by(nflId_def) %>%
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
            interceptions_created_per_target = interceptions_created/targets,
            expected_epa = sum(epa_pass_arrived),
            actual_epa = sum(epa_throw),
            epa_pass_arrived_avg = mean(epa_pass_arrived),
            epa_result_avg = mean(epa_throw),
            eps_saved_ball_skills = expected_epa - actual_epa,
            eps_saved_ball_skills_per_target = eps_saved_ball_skills/targets,
            eps_int_returns = -1*sum(epa_yaint, na.rm = TRUE)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, targets, everything()) %>%
  left_join(wr_db_zone_matchups_tot %>%
              group_by(nflId) %>%
              summarize(count = n()) %>%
              filter(count >= 100) %>%
              distinct(nflId) %>%
              mutate(qualifying = 1),
            by = c("nflId_def" = "nflId")) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), desc(eps_saved_ball_skills)) %>%
  left_join(ball_hawk_stats %>%
              rename(nflId_def = nflId))

ball_skills_ability2[is.na(ball_skills_ability2)] = 0

ball_skills_ability2 %>%
  ggplot() +
  geom_point(aes(x = epa_pass_arrived_avg, y = eps_saved_ball_skills)) +
  geom_smooth(aes(x = epa_pass_arrived_avg, y = eps_saved_ball_skills), method = 'lm')


# Adding In Penalties -----------------------------------------------------

ball_skills_penalty = ball_skills_defensive_penalties_score %>%
  mutate(epa_penalty = my_epa - mean(pass_arrived_epa$epa_pass_arrived)) %>%
  group_by(nflId_def) %>%
  summarize(ball_skills_penalties = n(),
            avg_ball_skills_penalties_eps = -1*mean(epa_penalty),
            ball_skills_penalties_eps = -1*sum(epa_penalty))


ball_skills_ability3 = ball_skills_ability2 %>%
  left_join(ball_skills_penalty) %>%
  left_join(pbus)

ball_skills_ability3[is.na(ball_skills_ability3)] = 0  

ball_skills_ability3 = ball_skills_ability3 %>%
  inner_join(expected_ball_hawk_eps) %>%
  mutate(eps_int_returns = eps_int_returns + ball_hawk_eps_int_returns) %>%
  mutate(eps_saved_ball_skills_per_target_w_penalties = (eps_saved_ball_skills_per_target*targets + 
                                                      ball_skills_penalties*avg_ball_skills_penalties_eps)/(ball_skills_penalties + targets),
         eps_saved_ball_skills_w_penalties = eps_saved_ball_skills + ball_skills_penalties*avg_ball_skills_penalties_eps + 
           ball_skills_defensive_penalties_man_avg*(ball_skills_penalties + targets),
         hands_on_ball_plays = interceptions + PB,
         hands_on_ball_rate = hands_on_ball_plays/(targets + ball_skills_penalties),
         eps_ball_hawk = ball_hawk_ints_eps + ball_hawk_pbus_eps - e_ball_hawk_eps) %>%
  arrange(desc(qualifying), desc(eps_saved_ball_skills_w_penalties))

write.csv(ball_skills_ability3,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_ball_skills_eps.csv",
          row.names = FALSE)

write.csv(player_extremes_ball_skill_ability,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_ball_skills_eps_plays_viz.csv",
          row.names = FALSE)

write.csv(player_extremes_ball_hawk,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_ball_hawk_eps_plays_viz.csv",
          row.names = FALSE)

write.csv(player_extremes_int_returns,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/dashbaord_player_int_returns_eps_plays_viz.csv",
          row.names = FALSE)

# Breaking Down By Route --------------------------------------------------

pbus = interceptions_incompletions_remove %>%
  inner_join(routes, 
             by = c("gameId", "playId", "nflId_off" = "nflId")) %>%
  filter(is_player & is.na(interception_player_name)) %>%
  group_by(route, nflId_def) %>%
  summarize(PB = n())

routes_ball_skills_ability = ball_skills_ability %>%
  inner_join(routes, 
             by = c("gameId", "playId", "targetNflId" = "nflId")) %>%
  group_by(route, nflId_def) %>%
  summarize(targets = n(),
            completions = sum(passResult == 'C'),
            interceptions = sum(passResult == 'IN'),
            expected_epa = sum(epa_pass_arrived),
            actual_epa = sum(epa_throw),
            epa_pass_arrived_avg = mean(epa_pass_arrived),
            epa_result_avg = mean(epa_throw),
            eps_saved_ball_skills = expected_epa - actual_epa,
            eps_saved_ball_skills_per_target = eps_saved_ball_skills/targets) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, route, targets, 
                completions, interceptions, eps_saved_ball_skills, eps_saved_ball_skills_per_target) %>%
  arrange(displayName, route, desc(eps_saved_ball_skills))

routes_ball_skills_ability2 = routes_ball_skills_ability %>%
  left_join(pbus)

routes_ball_skills_ability2[is.na(routes_ball_skills_ability2)] = 0  


write.csv(routes_ball_skills_ability2,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_ball_skills_eps_by_route.csv",
          row.names = FALSE)

