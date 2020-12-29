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
library(latex2exp)


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
  # group_by(gameId, playId, targetNflId) %>%
  # summarize(max_epa = max(epa_pass_attempt)) %>%
  inner_join(my_epa %>%
               dplyr::select(-my_ep)) %>%
  mutate(max_epa_disc = floor(epa_pass_attempt*4)/4 + .125) %>%
  group_by(max_epa_disc) %>%
  summarize(count = n(),
            avg_my_epa = mean(my_epa)) %>%
  arrange(max_epa_disc) %>%
  filter(count >= 50) %>%
  filter(max_epa_disc >= -2,
         max_epa_disc <= 2)

library(scam)
fitted.values = scam(avg_my_epa ~ s(max_epa_disc, k = 10, bs = "mpi"),
                     # weights = count,
                     data = epa_to_eps_per_play)$fitted.values

epa_to_eps_model = scam(avg_my_epa ~ s(max_epa_disc, k = 10, bs = "mpi"),
                        # weights = count,
                        data = epa_to_eps_per_play)

epa_to_eps_per_play$fitted_avg_my_epa = fitted.values

my_theme = theme_minimal() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

p1 = epa_to_eps_per_play %>%
  ggplot() +
  geom_point(aes(x = max_epa_disc, y = avg_my_epa),
             pch = 21, fill = "darkgray", color = "black", size = 2) +
  geom_line(aes(x =max_epa_disc, y = fitted.values), color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = TeX("$\\EPA^{route}$"),
       y = TeX("$\\widehat{EPA^{Play}}$"),
       title = "Average EPA of a Play vs. EPA of an Individual Route",
       subtitle = "") +
  my_theme

p1

avg_epa_per_passing_play = mean((epa_tracking_total %>%
                                   inner_join(wr_db_man_matchups,
                                              by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
                                   group_by(gameId, playId, targetNflId) %>%
                                   inner_join(my_epa %>%
                                                dplyr::select(-my_ep)))$my_epa)


epa_to_eps_per_play2 = epa_to_eps_per_play %>%
  mutate(fitted_avg_eps = avg_epa_per_passing_play - fitted_avg_my_epa)

epa_to_eps_per_play2 %>%
  ggplot() +
  geom_line(aes(x =max_epa_disc, y = fitted_avg_eps), color = "blue")

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

full_probs_df = full_probs_df %>%
  mutate(cumprob = cumsum(prob),
         cumprob_weights = cumprob/sum(cumprob))


p2 = full_probs_df %>%
  ggplot() +
  geom_density(aes(x = time_after_snap,
                y = cumprob),
               stat = "identity",
               fill = "darkblue",
               alpha = .6) +
  xlim(0, 7) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = TeX("t (s)"),
       y = TeX("$$F(t)$$"),
       title = "Cumulative Probability of Time After Snap of Pass Attempt") +
  my_theme

gridExtra::grid.arrange(p1, p2, ncol = 2)

g1 = gridExtra::arrangeGrob(p1, p2, ncol = 2)

ggsave(g1,
       filename = "~/Desktop/CoverageNet/src/08_writeup/images/PlayerTrackingPhysics2.png",
       height = 4,
       width = 12)


# Closing Metrics ---------------------------------------------------------

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

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

pass_arrived_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

closest_pairs = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/defense_off_closest_players.csv")

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
  left_join(wr_db_man_matchups,
            by = c("gameId", "playId", "nflId" = "nflId_off")) %>%
  left_join(wr_db_man_matchups,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  filter(!(is.na(nflId_def) & is.na(nflId_off))) %>%
  mutate(nflId_def2 = if_else(is.na(nflId_def), 
                              nflId,
                              nflId_def)) %>%
  filter(penaltyCodes == "DPI",
         (enforced_flag & !declined_flag & !offsetting_flag & (ball_skills_dpi == 0))) %>%
  dplyr::select(gameId, playId, nflId_def2, penaltyCodes, enforced_flag, declined_flag, offsetting_flag, my_epa) %>%
  rename(nflId_def = nflId_def2)


closing_defensive_penalties_man_avg = pass_attempt_epa %>%
  inner_join(targeted_receiver) %>%
  left_join(wr_db_man_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  left_join(closing_defensive_penalties_remove %>%
              filter(penaltyCodes == "DPI")) %>%
  ungroup() %>%
  summarize(avg_penalty_epa_per_target = mean(replace_na(my_epa, 0)))


# Finding Closest Player at Arrival ---------------------------------------

pass_arrived_closest_link = pass_arrived_epa %>%
  inner_join(pass_arrived_frames) %>%
  inner_join(targeted_receiver) %>%
  inner_join(closest_pairs,
             by = c("gameId", "playId", "targetNflId" = "nflId_opp")) %>%
  filter(frameId >= frameId_start,
         frameId <= frameId_end) %>%
  dplyr::select(gameId, playId, nflId, targetNflId) %>%
  rename(nflId_def = nflId,
         nflId_off = targetNflId)

# Joining the Data --------------------------------------------------------

# link to closest player removes ~10% of observations
closing_ability = pass_attempt_epa %>%
  inner_join(pass_arrived_epa) %>%
  inner_join(targeted_receiver) %>%
  filter(!is.na(targetNflId)) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  inner_join(pass_arrived_closest_link %>%
               distinct(gameId, playId, nflId_def))

closing_ability$fitted_epa_pass_arrived = lm(epa_pass_arrived ~ epa_pass_attempt, data = closing_ability)$fitted.values
closing_ability$fitted_comp_prob_pass_arrived = lm(comp_prob_pass_arrived ~ comp_prob_pass_attempt, data = closing_ability)$fitted.values

summary(lm(epa_pass_arrived ~ epa_pass_attempt, data = closing_ability))
summary(lm(comp_prob_pass_arrived ~ comp_prob_pass_attempt, data = closing_ability))

closing_ability %>%
  ggplot() +
  geom_point(aes(x = epa_pass_attempt, y = epa_pass_arrived),
             pch = 21, fill = "grey", color = "black", alpha = .6) +
  geom_line(aes(x = epa_pass_attempt, y = fitted_epa_pass_arrived), 
            color = "darkblue",
            size = 1) +
  labs(x = TeX("EPA at Pass Attempt"),
       y = TeX("EPA at Pass Arrival"),
       title = "EPA at Pass Arrival vs. at Pass Attempt",
       subtitle = "EPA at Pass Arrival is inflated due to exclusion of incompletions \nwithout a pass arrival flag") +
  my_theme

closing_ability_man = closing_ability %>%
  mutate(Coverage = "Coverage: Man")

players = read.csv("~/Desktop/CoverageNet/inputs/players.csv")
games = read.csv("~/Desktop/CoverageNet/inputs/games.csv")
plays = read.csv("~/Desktop/CoverageNet/inputs/plays.csv", stringsAsFactors = FALSE)
targeted_receiver = read.csv("~/Desktop/CoverageNet/inputs/targetedReceiver.csv")
routes = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/routes.csv")

wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")
wr_db_zone_matchups_tot = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/zone_defense_off_coverage_assignments_all_lbs.csv")

# pass attempt data
pass_attempt_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempt_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_attempt) %>%
  rename(comp_prob_pass_attempt = C_prob)

pass_attempt_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/02_score_attempt/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)


# pass arrived data
pass_arrived_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_arrived_epa_data.csv") %>%
  dplyr::select(gameId, playId, C_prob, epa_pass_arrived) %>%
  rename(comp_prob_pass_arrived = C_prob)

pass_arrived_frames = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/01_score_arrived/outputs/pass_attempts_with_fumbles.csv") %>%
  distinct(gameId, playId, frameId)

dpi_classification = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/pass_interference_classification/outputs/dpi_classification.csv")

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")

player_numbers = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/player_numbers.csv")


# Creating WR/DB zone links across time frames ------------------------------

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

# wr_db_zone_matchups = pass_attempt_zone_link %>%
#  inner_join(pass_arrived_zone_link)

wr_db_zone_matchups = pass_arrived_zone_link

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
  left_join(wr_db_zone_matchups,
            by = c("gameId", "playId", "nflId" = "nflId_off")) %>%
  left_join(wr_db_zone_matchups,
            by = c("gameId", "playId", "nflId" = "nflId_def")) %>%
  filter(!(is.na(nflId_def) & is.na(nflId_off))) %>%
  mutate(nflId_def2 = if_else(is.na(nflId_def), 
                              nflId,
                              nflId_def)) %>%
  filter(penaltyCodes == "DPI",
         (enforced_flag & !declined_flag & !offsetting_flag & (ball_skills_dpi == 0))) %>%
  dplyr::select(gameId, playId, nflId_def2, penaltyCodes, enforced_flag, declined_flag, offsetting_flag, my_epa) %>%
  rename(nflId_def = nflId_def2)


closing_defensive_penalties_man_avg = pass_attempt_epa %>%
  inner_join(targeted_receiver) %>%
  left_join(wr_db_zone_matchups,
            by = c("gameId", "playId", "targetNflId" = "nflId_off")) %>%
  left_join(closing_defensive_penalties_remove %>%
              filter(penaltyCodes == "DPI")) %>%
  ungroup() %>%
  summarize(avg_penalty_epa_per_target = mean(replace_na(my_epa, 0)))

# Joining the Data --------------------------------------------------------

closing_ability = pass_attempt_epa %>%
  inner_join(pass_arrived_epa) %>%
  inner_join(targeted_receiver) %>%
  filter(!is.na(targetNflId)) %>%
  inner_join(wr_db_zone_matchups %>%
               rename(targetNflId = nflId_off))

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
  geom_point(aes(x = epa_pass_attempt, y = epa_pass_arrived),
             pch = 21, fill = "grey", color = "black", alpha = .6) +
  geom_line(aes(x = epa_pass_attempt, y = fitted_epa_pass_arrived), 
            color = "darkblue",
            size = 1) +
  labs(x = TeX("EPA at Pass Attempt"),
       y = TeX("EPA at Pass Arrival"),
       title = "EPA at Pass Arrival vs. at Pass Attempt",
       subtitle = "EPA at Pass Arrival is inflated due to exclusion of incompletions \nwithout a pass arrival flag") +
  my_theme

closing_ability_zone = closing_ability %>%
  mutate(Coverage = "Coverage: Zone")

closing_ability_tot = rbind(closing_ability_man,
                            closing_ability_zone)

closing_ability_tot %>%
  ggplot() +
  geom_point(aes(x = epa_pass_attempt, y = epa_pass_arrived),
             pch = 21, fill = "grey", color = "black", alpha = .6) +
  geom_line(aes(x = epa_pass_attempt, y = fitted_epa_pass_arrived), 
            color = "darkblue",
            size = 1) +
  labs(x = TeX("EPA at Pass Attempt"),
       y = TeX("EPA at Pass Arrival"),
       title = "EPA at Pass Arrival vs. at Pass Attempt",
       subtitle = "EPA at Pass Arrival is inflated due to exclusion of incompletions without a pass arrival flag") +
  my_theme + 
  facet_wrap(~Coverage)

ggsave(filename = "~/Desktop/CoverageNet/src/08_writeup/images/PlayerClosingPhysics.png",
       height = 4,
       width = 8)


