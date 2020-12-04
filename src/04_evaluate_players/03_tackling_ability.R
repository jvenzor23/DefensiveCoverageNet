# This code scores player's tackling ability
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

wr_db_man_matchups = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments.csv")

pass_result_epa = read.csv("~/Desktop/CoverageNet/src/03_coverageNet/00_score_YAC/outputs/yac_yaint_epa_data.csv")

dpi_classification = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/pass_interference_classification/outputs/dpi_classification.csv")

my_epa = read.csv("~/Desktop/CoverageNet/src/02_yards_to_epa_function/outputs/plays_with_epa.csv")


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

fumbles_forced = fumbles_forced_remove %>%
  filter(is_player) %>%
  group_by(nflId_def) %>%
  summarize(FF = n())

tackles = wr_db_man_matchups %>%
  inner_join(targeted_receiver,
             by = c("gameId", "playId", "nflId_off" = "targetNflId")) %>%
  inner_join(plays %>%
               filter(passResult %in% c("C"),
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
  mutate(is_solo = replace_na(grepl(str_to_lower(solo_tackle_1_player_name), str_to_lower(displayNameNew)), FALSE),
         is_assist = replace_na(grepl(str_to_lower(assist_tackle_1_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(assist_tackle_2_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(assist_tackle_3_player_name), str_to_lower(displayNameNew)), FALSE)|
           replace_na(grepl(str_to_lower(assist_tackle_4_player_name), str_to_lower(displayNameNew)), FALSE)) %>%
  group_by(nflId_def) %>%
  summarize(Tackles = sum(1*is_solo) + .5*sum(1*is_assist))



# Identifying Penalties to Remove -----------------------------------------

# offsetting penalties/decline penalties:
# we cannot identify which period these are in, so they will be removed!

plays_no_penalties = plays %>%
  filter(penaltyCodes == "")


# Scoring All Plays -------------------------------------------------------

tackling_ability = pass_result_epa %>%
  filter(!is.na(epa_yac)) %>%
  inner_join(targeted_receiver) %>%
  inner_join(wr_db_man_matchups %>%
               rename(targetNflId = nflId_off)) %>%
  inner_join(plays_no_penalties %>%
               distinct(gameId, playId)) %>%
  anti_join(fumbles_forced_remove2) %>%
  group_by(nflId_def) %>%
  summarize(tackling_opportunities = n(),
            eps_tackling = -1*sum(epa_yac),
            eps_per_tackling_opportunity = -1*mean(epa_yac),
            perc_reduce_yac = mean(epa_yac < 0)) %>%
  left_join(fumbles_forced) %>%
  left_join(tackles) %>%
  inner_join(players %>%
               dplyr::select(nflId, displayName, position),
             by = c("nflId_def" = "nflId")) %>%
  left_join(wr_db_man_matchups %>%
              group_by(nflId_def) %>%
              summarize(count = n()) %>%
              filter(count >= 100) %>%
              distinct(nflId_def) %>%
              mutate(qualifying = 1)) %>%
  mutate(qualifying = replace_na(qualifying, 0)) %>%
  dplyr::select(qualifying, position, displayName, nflId_def, everything()) %>%
  arrange(desc(qualifying), desc(eps_tackling))

tackling_ability[is.na(tackling_ability)] = 0

tackling_ability = tackling_ability %>%
  mutate(tackle_perc = Tackles/tackling_opportunities)

write.csv(tackling_ability,
          "~/Desktop/CoverageNet/src/04_evaluate_players/outputs/player_tackling_epas.csv",
          row.names = FALSE)
            