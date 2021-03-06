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
wr_db_zone_matchups_tot = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")


# setwd("~/Desktop/NFL_PBP_DATA/")
# pbp_data_2018 = read_csv("reg_pbp_2018.csv", col_types = cols()) %>%
#   select(play_id, game_id, air_yards, yards_after_catch, return_yards, fumble_lost) %>%
#   rename(gameId = game_id,
#          playId = play_id)

pt_data = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/outputs/week1.csv")
my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

plays %>% group_by(penaltyCodes) %>% summarize(count = n()) %>% arrange(desc(count))

player_numbers = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/player_numbers.csv")
routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

check_event = pt_data %>%
  inner_join(plays) %>%
  filter(event != "None") %>%
  distinct(gameId, playId, passResult, event) %>%
  group_by(passResult, event) %>%
  summarize(count = n()) %>%
  arrange(passResult, desc(count))

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

tracking_defensive_penalties_man_avg = wr_db_zone_matchups_tot %>%
  left_join(tracking_defensive_penalties2 %>%
            anti_join(wr_db_man_matchups),
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  ungroup() %>%
  summarize(avg_penalty_epa_per_route = mean(replace_na(my_epa, 0)))


# Removing Penalty Plays From Scoring Data Set ----------------------------

epa_tracking_total_penalties_removed = epa_tracking_total %>%
  inner_join(wr_db_zone_matchups_tot,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(tracking_defensive_penalties2,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  group_by(gameId, playId, nflId, targetNflId, frameId_start) %>%
  mutate(nFrames = max(frameId) - min(frameId) + 1) %>%
  filter(nFrames >= 5) %>%
  dplyr::select(-nFrames)

# Translating epa_per_route to eps_per_play -------------------------------

epa_to_eps_per_play = epa_tracking_total_penalties_removed %>%
  group_by(gameId, playId, targetNflId, nflId, frameId_start) %>%
  # summarize(max_epa = max(epa_pass_attempt)) %>%
    # max_epa = epa_pass_attempt[length(epa_pass_attempt)]) %>%
  # group_by(gameId, playId, targetNflId) %>%
  # filter(frameId_start == max(frameId_start)) %>%
  inner_join(my_epa %>%
               dplyr::select(-my_ep)) %>%
  mutate(max_epa_disc = floor(epa_pass_attempt*4)/4 + .125) %>%
  group_by(max_epa_disc) %>%
  summarize(count = n(),
            avg_my_epa = mean(my_epa)) %>%
  arrange(max_epa_disc) %>%
  filter(count >= 150)

library(scam)
fitted.values = scam(avg_my_epa ~ s(max_epa_disc, k = 5, bs = "mpi"),
                     # weights = count,
                     data = epa_to_eps_per_play)$fitted.values

epa_to_eps_model = scam(avg_my_epa ~ s(max_epa_disc, k = 5, bs = "mpi"),
                        # weights = count,
                        data = epa_to_eps_per_play)

epa_to_eps_per_play$fitted_avg_my_epa = fitted.values

epa_to_eps_per_play %>%
  ggplot() +
  geom_point(aes(x = max_epa_disc, y = avg_my_epa)) +
  geom_line(aes(x =max_epa_disc, y = fitted.values), color = "blue")

avg_epa_per_passing_play = mean((epa_tracking_total %>%
                                   inner_join(wr_db_zone_matchups_tot,
                                              by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
                                   filter(frameId >= frameId_start,
                                          frameId <= frameId_end) %>%
                                   group_by(gameId, playId, nflId, targetNflId, frameId_start) %>%
                                   mutate(nFrames = max(frameId) - min(frameId) + 1) %>%
                                   filter(nFrames >= 5) %>%
                                   dplyr::select(-nFrames) %>%
                                   inner_join(my_epa %>%
                                                dplyr::select(-my_ep)))$my_epa)


epa_to_eps_per_play2 = epa_to_eps_per_play %>%
  mutate(fitted_avg_eps = avg_epa_per_passing_play - fitted_avg_my_epa)

epa_to_eps_per_play2 %>%
  ggplot() +
  geom_line(aes(x =max_epa_disc, y = fitted_avg_eps), color = "blue")


# Getting Throw Time Dist -------------------------------------------------

time_to_hold_dist =  epa_tracking_total %>%
  inner_join(wr_db_zone_matchups_tot,
                                by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  anti_join(tracking_defensive_penalties2,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  group_by(gameId, playId, nflId, targetNflId, frameId_start) %>%
  anti_join(plays %>%
              filter(passResult == 'S') %>%
              distinct(gameId, playId)) %>%
  arrange(gameId, playId, targetNflId, nflId, frameId) %>%
  group_by(gameId, playId, targetNflId, nflId, frameId_start) %>%
  mutate(time_after_entering_zone = ((frameId - min(frameId)) + 1)*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, nflId, targetNflId) %>%
  summarize(time_to_hold = max(time_after_entering_zone))

# sn_fit = fitdist(time_to_hold_dist$time_to_hold,
#                  distr = "sn",
#                  method = "mse",
#                  start = list("tau" = 0, "omega" = 2, "alpha" = 2))$estimate

time_to_hold_dist2 = time_to_hold_dist %>%
  group_by(time_to_hold) %>%
  summarize(count = n()) %>%
  mutate(perc = count/sum(count))

# sn_fitted_vals = dsn(x = time_to_hold_dist2$time_to_hold, xi=0, omega=sn_fit[2], alpha=sn_fit[3], tau=sn_fit[1], dp=NULL, log=FALSE)

time_to_hold_dist2$fitted_perc = time_to_hold_dist2$perc/sum(time_to_hold_dist2$perc)

time_to_hold_dist2 %>%
  ggplot() +
  geom_density(stat = "identity", aes(x = time_to_hold, y = fitted_perc),
               fill = "blue", alpha = .6) +
  geom_density(stat = "identity", aes(x = time_to_hold, y = perc)) +
  labs(x = "Time To Hold Zone",
       y = "Probability Density")


# Rating Defensive Backs --------------------------------------------------

# full_probs = dsn(x = seq(0, 15, .1), xi=0, omega=sn_fit[2], alpha=sn_fit[3], tau=sn_fit[1], dp=NULL, log=FALSE)/10

# full_probs_df = data.frame("time_after_entering_zone" = seq(0, 15, .1),
#                            "prob" = full_probs)

full_probs_df = time_to_hold_dist2 %>%
  dplyr::select(time_to_hold,
                fitted_perc) %>%
  rename(time_after_entering_zone = time_to_hold,
         prob = fitted_perc)

# Looking at Max EPA over Each Interval Per Play --------------------------

max_zone_coverage_tracking = epa_tracking_total_penalties_removed %>%
  arrange(gameId, playId, targetNflId, nflId, frameId) %>%
  group_by(gameId, playId, targetNflId, nflId, frameId_start) %>%
  mutate(time_after_entering_zone = ((frameId - min(frameId) + 1))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, targetNflId, nflId, frameId_start) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() 

max_zone_coverage_tracking$eps_tracking_avg = predict(epa_to_eps_model,
                                                      max_zone_coverage_tracking %>%
                                                       rename(max_epa_disc = epa_pass_attempt_max))

max_zone_coverage_tracking = max_zone_coverage_tracking %>%
  mutate(eps_tracking_per_play = avg_epa_per_passing_play - eps_tracking_avg)

max_zone_coverage_tracking =  max_zone_coverage_tracking %>%
  group_by(nflId, time_after_entering_zone) %>%
  summarise(count = n(),
            avg_max_epa = mean(epa_pass_attempt_max),
            avg_eps_tracking_per_play = mean(eps_tracking_per_play)) %>%
  arrange(nflId, time_after_entering_zone)

full_probs_df = full_probs_df %>%
  mutate(cumprob = 1- cumsum(prob),
         cumprob_weights = cumprob/sum(cumprob))

max_zone_coverage_tracking_filled2 = max_zone_coverage_tracking %>%
  mutate(time_after_entering_zone = as.factor(time_after_entering_zone)) %>%
  inner_join(full_probs_df %>%
               mutate(time_after_entering_zone = as.factor(time_after_entering_zone))) %>%
  ungroup() %>%
  filter(as.numeric(time_after_entering_zone) >= .5) %>%
  group_by(nflId) %>%
  mutate(prob_norm = cumprob_weights/sum(cumprob_weights)) %>%
  group_by(nflId) %>%
  summarize(routes = max(count),
            normalized_avg_max_epa = sum(avg_max_epa*prob_norm),
            eps_tracking_per_play = sum(avg_eps_tracking_per_play*prob_norm)) %>%
  mutate(eps_tracking = eps_tracking_per_play*routes) %>%
  inner_join(players %>%
               dplyr::select(position, nflId, displayName),
             by = c("nflId" = "nflId")) %>%
  dplyr::select(position, displayName, nflId, routes, eps_tracking, eps_tracking_per_play, normalized_avg_max_epa) %>%
  arrange(desc(eps_tracking))


# Adding in Penalties -----------------------------------------------------

tracking_defensive_penalties3 = tracking_defensive_penalties2 %>%
  filter(enforced_flag & (!offsetting_flag)) %>%
  group_by(nflId_def) %>%
  summarize(penalities_count = n(),
            avg_epa_penalty = mean(my_epa),
            penalties_eps = -1*penalities_count*avg_epa_penalty) %>%
  arrange(desc(penalities_count))

max_zone_coverage_tracking_filled3 = max_zone_coverage_tracking_filled2 %>%
  left_join(tracking_defensive_penalties3,
            by = c("nflId" = "nflId_def")) %>%
  rename(nflId_def = nflId)

max_zone_coverage_tracking_filled3[is.na(max_zone_coverage_tracking_filled3)] = 0

max_zone_coverage_tracking_filled3 = max_zone_coverage_tracking_filled3 %>%
  mutate(penalties_and_time_to_throw_normalized_tracking_epa = (routes*normalized_avg_max_epa + 
                      penalities_count*avg_epa_penalty)/(routes + penalities_count),
         eps_tracking_w_penalties = eps_tracking + penalties_eps + 
           (tracking_defensive_penalties_man_avg$avg_penalty_epa_per_route)*(routes + avg_epa_penalty),
         tracking_penalty_perc = penalities_count/(penalities_count + routes)) %>%
  dplyr::select(position, displayName, nflId_def, routes, eps_tracking_w_penalties, eps_tracking, normalized_avg_max_epa, penalties_and_time_to_throw_normalized_tracking_epa,
                penalities_count, tracking_penalty_perc, penalties_eps) %>%
  arrange(penalties_and_time_to_throw_normalized_tracking_epa) %>%
  mutate(qualifying = if_else(routes > 75, 1, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), desc(eps_tracking_w_penalties))

# getting data for plotting
player_probs_vs_time = max_zone_coverage_tracking %>%
  mutate(time_after_entering_zone = as.factor(time_after_entering_zone)) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId" = "nflId")) %>%
  dplyr::select(position, displayName, nflId, everything()) %>%
  rename(nflId_def = nflId) %>%
  left_join(wr_db_zone_matchups_tot %>%
              group_by(nflId) %>%
              summarize(count = n()) %>%
              filter(count >= 100) %>%
              distinct(nflId) %>%
              mutate(qualifying = 1),
            by = c("nflId_def" = "nflId")) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), displayName)

# Writing the Data --------------------------------------------------------

write.csv(max_zone_coverage_tracking_filled3,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_tracking_eps.csv",
          row.names = FALSE)

write.csv(player_probs_vs_time,
          "~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/player_tracking_epa_by_time_after_snap.csv",
          row.names = FALSE)


# Analysis By Route -------------------------------------------------------

library(broom)

# Translating epa_per_route to eps_per_play -------------------------------

route_epa_to_eps_per_play = epa_tracking_total_penalties_removed %>%
  inner_join(routes) %>%
  group_by(gameId, playId, route, targetNflId) %>%
  summarize(max_epa = max(epa_pass_attempt)) %>%
  inner_join(my_epa %>%
               dplyr::select(-my_ep)) %>%
  mutate(max_epa_disc = floor(max_epa*4)/4 + .125) %>%
  group_by(route, max_epa_disc) %>%
  summarize(count = n(),
            avg_my_epa = mean(my_epa)) %>%
  arrange(route, max_epa_disc) %>%
  filter(count >= 10)

route_epa_to_eps_per_play_model = do(route_epa_to_eps_per_play %>% 
                                 group_by(route), 
                               tidy(lm(avg_my_epa ~ max_epa_disc,
                                          weights = count,
                                          data = .))) %>%
  dplyr::select(route, term, estimate) %>%
  pivot_wider(names_from = term,
              values_from = estimate)

route_epa_to_eps_per_play = do(route_epa_to_eps_per_play %>% 
                                                   group_by(route), 
                                                 augment(lm(avg_my_epa ~ max_epa_disc,
                                                            weights = count,
                                                            data = .))) %>%
  dplyr::select(route, max_epa_disc, avg_my_epa, .fitted) %>%
  rename(fitted_avg_my_epa = .fitted)


route_epa_to_eps_per_play %>%
  ggplot() +
  geom_point(aes(x = max_epa_disc, y = avg_my_epa)) +
  geom_line(aes(x =max_epa_disc, y = fitted_avg_my_epa), color = "blue") +
  facet_wrap(~route, scales = "free")

route_avg_epa_per_passing_play = epa_tracking_total_penalties_removed %>%
                                inner_join(routes) %>%
                                inner_join(my_epa %>%
                                      dplyr::select(-my_ep)) %>%
                                distinct(gameId, playId, route, my_epa) %>%
                                group_by(route) %>%
                                summarize(avg_epa = mean(my_epa))


# Looking at Max EPA over Each Interval Per Play --------------------------

route_max_man_coverage_tracking = epa_tracking_total_penalties_removed %>%
  inner_join(routes,
             by = c("gameId", "playId", "targetNflId" = "nflId")) %>%
  inner_join(wr_db_man_matchups,
             by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  group_by(gameId, playId, targetNflId) %>%
  mutate(time_after_snap = (5 + (frameId - min(frameId)))*.1) %>%
  ungroup() %>%
  group_by(gameId, playId, nflId_def, route) %>%
  mutate(epa_pass_attempt_max = cummax(epa_pass_attempt)) %>%
  ungroup() %>%
  group_by(nflId_def, route, time_after_snap) %>%
  summarise(count = n(),
            avg_max_epa = mean(epa_pass_attempt_max)) %>%
  # anti_join(wr_db_man_matchups %>%
  #             group_by(nflId_def) %>%
  #             summarize(count = n()) %>%
  #             filter(count < 100) %>%
  #             distinct(nflId_def)) %>%
  arrange(nflId_def, route, time_after_snap) %>%
  group_by(nflId_def, route) %>%
  mutate(max_time_for_route = max(time_after_snap)) %>%
  ungroup() %>%
  filter(count >= 3)

route_max_man_coverage_tracking_filled = route_max_man_coverage_tracking %>%
  ungroup() %>%
  complete(nesting(nflId_def, route), time_after_snap)

route_max_man_coverage_tracking_filled$count[is.na(route_max_man_coverage_tracking_filled$count)] = 1

route_max_man_coverage_tracking_filled = route_max_man_coverage_tracking_filled %>%
  fill(avg_max_epa) %>%
  fill(max_time_for_route)

# Fitting a Monotonic Function to Each Player -----------------------------

route_fitted_max_man_coverage_tracking = route_max_man_coverage_tracking_filled %>%
  inner_join(route_max_man_coverage_tracking %>%
               distinct(nflId_def, route) %>%
               ungroup() %>%
               mutate(group = row_number()))

# Break up d by state, then fit the specified model to each piece and
# return a list
models <- plyr::dlply(route_fitted_max_man_coverage_tracking, "group"
                      , function(df) 
  scam(avg_max_epa ~ s(time_after_snap, k = 12, bs = "mpi"), 
       weights = count,
       data = df))

# Apply coef to each model and return a data frame
route_fit_by_player = plyr::ldply(models, stats::fitted.values)

route_fit_by_player2 = route_fit_by_player %>%
  pivot_longer(cols = setdiff(names(route_fit_by_player), "group"),
               names_to = "time_after_snap_placeholder",
               values_to = "avg_max_epa_pred") %>%
  group_by(group) %>%
  mutate(time_after_snap = row_number()*.1 + .4) %>%
  dplyr::select(-time_after_snap_placeholder)


route_fitted_max_man_coverage_tracking2 = route_fitted_max_man_coverage_tracking %>%
  mutate(time_after_snap = as.factor(time_after_snap)) %>%
  inner_join(route_fit_by_player2 %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  inner_join(route_epa_to_eps_per_play_model) %>%
  mutate(eps_tracking_avg = `(Intercept)` + max_epa_disc*avg_max_epa_pred) %>%
  inner_join(route_avg_epa_per_passing_play) %>%
  mutate(eps_tracking_per_play = avg_epa - eps_tracking_avg)

route_fitted_max_man_coverage_tracking2 = route_fitted_max_man_coverage_tracking2 %>%
  inner_join(full_probs_df %>%
               mutate(time_after_snap = as.factor(time_after_snap))) %>%
  group_by(nflId_def, route) %>%
  mutate(prob_norm = prob/sum(prob)) %>%
  group_by(nflId_def, route) %>%
  summarize(routes = max(count),
            eps_tracking_per_play = sum(eps_tracking_per_play*prob_norm),
            normalized_avg_max_epa = sum(avg_max_epa_pred*prob_norm)) %>%
  inner_join(players %>%
               dplyr::select(position, nflId, displayName),
             by = c("nflId_def" = "nflId")) %>%
  dplyr::select(position, displayName, nflId_def, route, routes, eps_tracking_per_play, normalized_avg_max_epa) %>%
  left_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count >= 100) %>%
              distinct(nflId_def) %>%
              mutate(qualifying = 1)) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, everything()) %>%
  arrange(desc(qualifying), displayName, desc(eps_tracking_per_play))


write.csv(route_fitted_max_man_coverage_tracking2,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tracking_eps_by_route.csv",
          row.names = FALSE)


