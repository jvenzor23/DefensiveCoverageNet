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
epa_tracking_total = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/03_score_tracking/outputs/routes_tracking_epa.csv")
wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
# dropping WRs with double teams
wr_db_man_matchups = wr_db_man_matchups %>%
  group_by(gameId, playId, nflId_off) %>%
  filter(n() == 1)

# setwd("~/Desktop/NFL_PBP_DATA/")
# pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
#   select(play_id, game_id, air_yards, yards_after_catch, return_yards, fumble_lost) %>%
#   rename(gameId = game_id,
#          playId = play_id)

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")
pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_attempt) %>%
  rename(comp_prob_pass_attempt = C_prob)

plays %>% group_by(penaltyCodes) %>% summarize(count = n()) %>% arrange(desc(count))

player_numbers = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/player_numbers.csv")
routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

# Getting Penalties --------------------------------------------------------

plays_penalties = plays %>%
  filter(penaltyCodes != "") %>%
  dplyr::select(gameId, playId, playDescription, possessionTeam, penaltyCodes, penaltyJerseyNumbers,
                offensePlayResult, playResult, isDefensivePI) %>%
  inner_join(my_epa) %>%
  rowwise() %>%
  mutate(penalty_desc = trimws(str_split(str_split(str_to_lower(playDescription), "penalty")[[1]][2], ",")[[1]][2])) %>%
  mutate(enforced_flag = grepl("enforced", str_to_lower(playDescription)),
         declined_flag = grepl("declined", str_to_lower(playDescription)),
         offsetting_flag = grepl("offsetting", str_to_lower(playDescription)))

penalty_freq = plays_penalties %>%
  group_by(penaltyCodes, penalty_desc) %>%
  summarize(count = n(),
            perc_enforced = mean(enforced_flag),
            perc_declined = mean(declined_flag),
            perc_offsetting = mean(offsetting_flag),
            avg_epa = mean(my_epa, na.rm = TRUE)
  ) %>%
  arrange(desc(count))

check = plays_penalties %>%
  filter(penaltyCodes == "DH;OH")


# Breaking Out Penalty Codes and Jersey Numbers to Handle Multiple --------
plays_penalties2 = plays_penalties %>%
  separate_rows(penaltyCodes, penaltyJerseyNumbers, 
                sep = ";", convert = TRUE) %>%
  dplyr::select(gameId, playId, possessionTeam, penaltyCodes, penaltyJerseyNumbers,
                ends_with("flag"), my_epa) %>%
  rowwise() %>%
  mutate(isDefensivePenalty = (str_to_lower(possessionTeam) !=
                                 str_split(str_to_lower(penaltyJerseyNumbers),
                                           " ")[[1]][1]))

penalty_freq2 = plays_penalties2 %>%
  group_by(penaltyCodes) %>%
  summarize(count = n(),
            perc_isDefensivePenalty = mean(isDefensivePenalty),
            perc_enforced = mean(enforced_flag),
            perc_declined = mean(declined_flag),
            perc_offsetting = mean(offsetting_flag),
            avg_epa = mean(my_epa, na.rm = TRUE)
  ) %>%
  arrange(desc(count))

tracking_defensive_penalties = plays_penalties2 %>%
  filter(penaltyCodes %in% c('DH', 'ICT', 'ILHd', 'ILH'),
         isDefensivePenalty) %>%
  mutate(jerseyNumber = str_split(penaltyJerseyNumbers, " ")[[1]][2]) %>%
  dplyr::select(gameId, playId, possessionTeam, jerseyNumber, penaltyCodes, 
                enforced_flag, offsetting_flag, my_epa)

player_numbers2 = player_numbers %>%
  filter(isOnDefense)

tracking_defensive_penalties2 = tracking_defensive_penalties %>%
  inner_join(player_numbers2 %>%
               mutate(jerseyNumber = as.character(jerseyNumber))) %>%
  dplyr::select(gameId, playId, nflId, penaltyCodes, enforced_flag, offsetting_flag, my_epa) %>%
  rename(nflId_def = nflId)

tracking_defensive_penalties_man_avg = wr_db_man_matchups %>%
  left_join(tracking_defensive_penalties2) %>%
  ungroup() %>%
  summarize(avg_penalty_epa_per_route = mean(replace_na(my_epa, 0)))


# Removing Penalty Plays From Scoring Data Set ----------------------------

epa_tracking_total_penalties_removed = epa_tracking_total %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  anti_join(tracking_defensive_penalties2) %>%
  dplyr::select(names(epa_tracking_total))

# Translating epa_per_route to eps_per_play -------------------------------

epa_to_eps_per_play = epa_tracking_total_penalties_removed %>%
  group_by(gameId, playId, targetNflId) %>%
  summarize(epa_final = epa_pass_attempt[length(epa_pass_attempt)]) %>%
  inner_join(pass_attempt_epa) %>%
  mutate(epa_final_disc = floor(epa_final*4)/4 + .125) %>%
  group_by(epa_final_disc) %>%
  summarize(count = n(),
            avg_epa_pass_attempt = mean(epa_pass_attempt)) %>%
  arrange(epa_final_disc) %>%
  filter(count >= 100)

library(scam)
fitted.values = scam(avg_epa_pass_attempt ~ s(epa_final_disc, k = 5, bs = "mpi"),
                     # weights = count,
                     data = epa_to_eps_per_play)$fitted.values

epa_to_eps_model = scam(avg_epa_pass_attempt ~ s(epa_final_disc, k = 5, bs = "mpi"),
                        # weights = count,
                        data = epa_to_eps_per_play)

epa_to_eps_per_play$fitted_avg_my_epa = fitted.values

epa_to_eps_per_play %>%
  ggplot() +
  geom_point(aes(x = epa_final_disc, y = avg_epa_pass_attempt),
             pch = 21, fill = "grey", color = "black") +
  geom_line(aes(x =epa_final_disc, y = fitted.values), color = "darkblue") +
  labs(x = "Route EPA at Pass Attempt",
       y = "Average EPA of Pass Attempt") +
  theme_minimal()

avg_epa_per_passing_play = mean((epa_tracking_total %>%
                                   inner_join(wr_db_man_matchups,
                                              by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
                                   distinct(gameId, playId, nflId_def) %>%
                                   inner_join(pass_attempt_epa))$epa_pass_attempt)


epa_to_eps_per_play2 = epa_to_eps_per_play %>%
  mutate(fitted_avg_eps = avg_epa_per_passing_play - fitted_avg_my_epa)

epa_to_eps_per_play2 %>%
  ggplot() +
  geom_line(aes(x =epa_final_disc, y = fitted_avg_eps), color = "darkblue") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Route EPA at Pass Attempt",
       y = "Estimated EPS") +
  theme_minimal()

# Getting Man Coverage Win Rates ------------------------------------------

man_cov_win_rate = epa_tracking_total_penalties_removed %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, nflId_def) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  filter(time_after_snap == 2.5) %>%
  group_by(nflId_def) %>%
  summarize(count = n(),
            man_tracking_win_rate = mean(epa_pass_attempt < 0.2))

# Getting Throw Time Dist -------------------------------------------------

time_to_throw_dist = epa_tracking_total_penalties_removed %>%
  anti_join(plays %>%
              filter(passResult == 'S') %>%
              distinct(gameId, playId)) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  summarize(time_to_throw = max(time_after_snap))

sn_fit = fitdist(time_to_throw_dist$time_to_throw,
                 distr = "sn",
                 method = "mle",
                 start = list("tau" = 0, "omega" = 2.75, "alpha" = 3))$estimate

time_to_throw_dist2 = time_to_throw_dist %>%
  group_by(time_to_throw) %>%
  summarize(count = n()) %>%
  mutate(perc = count/sum(count))

sn_fitted_vals = dsn(x = time_to_throw_dist2$time_to_throw, xi=0, omega=sn_fit[2], alpha=sn_fit[3], tau=sn_fit[1], dp=NULL, log=FALSE)

time_to_throw_dist2$fitted_perc = sn_fitted_vals/10

time_to_throw_dist2 %>%
  ggplot() +
  geom_density(stat = "identity", aes(x = time_to_throw, y = fitted_perc),
               fill = "blue", alpha = .6) +
  geom_density(stat = "identity", aes(x = time_to_throw, y = perc)) +
  labs(x = "Time To Pass Attempt(s)",
       y = "Probability Density")


# Rating Defensive Backs --------------------------------------------------

full_probs = dsn(x = seq(0, 15, .1), xi=0, omega=sn_fit[2], alpha=sn_fit[3], tau=sn_fit[1], dp=NULL, log=FALSE)/10

full_probs_df = data.frame("time_after_snap" = seq(0, 15, .1),
                           "prob" = full_probs)

# Looking at Max EPA over Each Interval Per Play --------------------------

max_man_coverage_tracking = epa_tracking_total_penalties_removed %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1)


max_man_coverage_tracking$eps_tracking_avg = predict(epa_to_eps_model,
                                                     max_man_coverage_tracking %>%
                                                       rename(epa_final_disc = epa_pass_attempt))

max_man_coverage_tracking = max_man_coverage_tracking %>%
  mutate(eps_tracking_per_play = avg_epa_per_passing_play - eps_tracking_avg)

players_extreme_plays = max_man_coverage_tracking %>%
  dplyr::select(week, gameId, playId, nflId_def, targetNflId,
                time_after_snap, eps_tracking_per_play) %>%
  mutate(eps_tracking_per_play = round(eps_tracking_per_play, 3)) %>%
  rename(eps = eps_tracking_per_play) %>%
  arrange(nflId_def, time_after_snap, desc(eps)) %>%
  filter(time_after_snap %in% c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5))


max_man_coverage_tracking = max_man_coverage_tracking %>%
  ungroup() %>%
  group_by(nflId_def, time_after_snap) %>%
  summarise(count = n(),
            avg_epa = mean(epa_pass_attempt),
            avg_eps_tracking_per_play = mean(eps_tracking_per_play)) %>%
  arrange(nflId_def, time_after_snap)

full_probs_df = full_probs_df %>%
  mutate(cumprob = 1- cumsum(prob),
         cumprob_weights = cumprob/sum(cumprob))

fitted_max_man_coverage_tracking2 = max_man_coverage_tracking %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(full_probs_df %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  group_by(nflId_def) %>%
  mutate(prob_norm = prob/sum(prob)) %>%
  group_by(nflId_def) %>%
  summarize(routes = max(count),
            normalized_avg_max_epa = sum(avg_epa*prob_norm),
            eps_tracking_per_play = sum(avg_eps_tracking_per_play*prob_norm)) %>%
  mutate(eps_tracking = eps_tracking_per_play*routes) %>%
  inner_join(players %>%
               dplyr::select(position, nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, routes, eps_tracking, eps_tracking_per_play, normalized_avg_max_epa) %>%
  arrange(desc(eps_tracking))


# Adding in Penalties -----------------------------------------------------

tracking_defensive_penalties3 = tracking_defensive_penalties2 %>%
  filter(enforced_flag & (!offsetting_flag)) %>%
  group_by(nflId_def) %>%
  summarize(tracking_penalities_count = n(),
            avg_epa_penalty = mean(my_epa),
            tracking_penalties_eps = -1*tracking_penalities_count*avg_epa_penalty) %>%
  arrange(desc(tracking_penalities_count))

fitted_max_man_coverage_tracking3 = fitted_max_man_coverage_tracking2 %>%
  left_join(tracking_defensive_penalties3) %>%
  left_join(man_cov_win_rate)

fitted_max_man_coverage_tracking3[is.na(fitted_max_man_coverage_tracking3)] = 0

fitted_max_man_coverage_tracking3 = fitted_max_man_coverage_tracking3 %>%
  mutate(penalties_and_time_to_throw_normalized_tracking_epa = (routes*normalized_avg_max_epa + 
                                                                  tracking_penalities_count*avg_epa_penalty)/(routes + tracking_penalities_count),
         eps_tracking_w_penalties = eps_tracking + tracking_penalties_eps + 
           (tracking_defensive_penalties_man_avg$avg_penalty_epa_per_route)*(routes + avg_epa_penalty),
         tracking_penalty_perc = tracking_penalities_count/(tracking_penalities_count + routes)) %>%
  dplyr::select(position, displayName, nflId_def, routes, eps_tracking_w_penalties, eps_tracking, normalized_avg_max_epa, penalties_and_time_to_throw_normalized_tracking_epa,
                tracking_penalities_count, tracking_penalty_perc, tracking_penalties_eps, man_tracking_win_rate) %>%
  arrange(penalties_and_time_to_throw_normalized_tracking_epa) %>%
  left_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count >= 100) %>%
              distinct(nflId_def) %>%
              mutate(qualifying = 1)) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), desc(eps_tracking_w_penalties))

# getting data for plotting
player_probs_vs_time = max_man_coverage_tracking %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, everything()) %>%
  left_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count >= 100) %>%
              distinct(nflId_def) %>%
              mutate(qualifying = 1)) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), displayName) %>%
  ungroup() %>%
  mutate(time_after_snap = as.numeric(as.character(time_after_snap))) %>%
  filter(time_after_snap <= 6)

write.csv(fitted_max_man_coverage_tracking3,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tracking_eps.csv",
          row.names = FALSE)

write.csv(player_probs_vs_time,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashboard_player_tracking_eps_by_time_after_snap.csv",
          row.names = FALSE)

write.csv(players_extreme_plays,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/dashboard_player_tracking_eps_plays_viz.csv",
          row.names = FALSE)

# Analysis By Route -------------------------------------------------------

library(broom)

# Translating epa_per_route to eps_per_play -------------------------------

max_man_coverage_tracking = epa_tracking_total_penalties_removed %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1)


max_man_coverage_tracking$eps_tracking_avg = predict(epa_to_eps_model,
                                                     max_man_coverage_tracking %>%
                                                       rename(epa_final_disc = epa_pass_attempt))

max_man_coverage_tracking = max_man_coverage_tracking %>%
  mutate(eps_tracking_per_play = avg_epa_per_passing_play - eps_tracking_avg)

route_max_man_coverage_tracking = max_man_coverage_tracking %>%
  inner_join(routes, 
             by = c("gameId", "playId", "targetNflId" = "nflId")) %>%
  filter(route != "undefined")

route_max_man_coverage_tracking = route_max_man_coverage_tracking %>%
  group_by(nflId_def, route, time_after_snap) %>%
  summarise(count = n(),
            avg_epa = mean(epa_pass_attempt),
            avg_eps_tracking = mean(eps_tracking_per_play)) %>%
  arrange(nflId_def, route, time_after_snap)


route_fitted_max_man_coverage_tracking2 = route_max_man_coverage_tracking %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(full_probs_df %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  group_by(nflId_def, route) %>%
  mutate(prob_norm = prob/sum(prob)) %>%
  group_by(nflId_def, route) %>%
  summarize(routes = max(count),
            eps_tracking_per_play = sum(avg_eps_tracking*prob_norm),
            normalized_avg_max_epa = sum(avg_epa*prob_norm),
            eps_tracking = eps_tracking_per_play*routes) %>%
  mutate(eps_tracking = eps_tracking_per_play*routes) %>%
  inner_join(players %>%
               dplyr::select(position, nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, route, routes, eps_tracking, eps_tracking_per_play, normalized_avg_max_epa) %>%
  arrange(desc(eps_tracking))

write.csv(route_fitted_max_man_coverage_tracking2,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tracking_eps_by_route.csv",
          row.names = FALSE)
