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

man_ratings = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/overall_player_skills_summary.csv")
zone_ratings = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/overall_player_skills_summary.csv")
sos_ratings = read.csv("~/Desktop/CoverageNet/src/06_evaluate_receivers/outputs/overall_player_sos_skills_summary.csv")

player_info = read.csv("~/Desktop/CoverageNet/src/00_data_wrangle/helper_tables/dashboard_player_info.csv") %>%
  dplyr::select(nflId, displayName, team, games, plays, position) %>%
  rename(pass_snaps = plays,
         Pos = position,
         G = games,
         `Pass Snaps` = plays) %>%
  mutate(Player = paste0(displayName, " (", team, ")")) %>%
  dplyr::select(nflId, Player, G, `Pass Snaps`, Pos)

man_zone_classification = rbind(
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_pass_attempts_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability),
  read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/all_positions_sacks_man_zone_classes.csv") %>%
    dplyr::select(gameId, playId, nflId, zone_probability)
) %>%
  arrange(gameId, playId, nflId) %>%
  distinct(gameId, playId, nflId, .keep_all = TRUE)

man_coverage = read.csv("~/Desktop/CoverageNet/src/01_identify_man_coverage/outputs/man_defense_off_coverage_assignments_all_lbs.csv")

zone_coverage = man_zone_classification %>%
  anti_join(man_coverage,
            by = c("gameId", "playId", "nflId" = "nflId_def"))

man_zone_perc = man_coverage %>%
  distinct(gameId, playId, nflId_def) %>%
  group_by(nflId_def) %>%
  summarize(man_plays = n()) %>%
  full_join(zone_coverage %>%
              rename(nflId_def = nflId) %>%
              distinct(gameId, playId, nflId_def) %>%
              group_by(nflId_def) %>%
              summarize(zone_plays = n()))

man_zone_perc[is.na(man_zone_perc)] = 0

man_zone_perc = man_zone_perc %>%
  mutate(`%Man` = man_plays/(zone_plays + man_plays)) %>%
  dplyr::select(nflId_def, `%Man`)


# Overall Aggregation ---------------------------------------------

overall_table = man_ratings %>%
  dplyr::select(nflId_def, eps_man_coverage, routes, accurate_targets, completions_allowed,
                PB, ball_hawk_pbus, INT, ball_hawk_ints, T, FF, penalties_count,
                penalties_eps) %>%
  rename(man_routes = routes,
         man_accurate_targets = accurate_targets,
         man_completions_allowed = completions_allowed,
         man_T = T,
         man_INTs = INT,
         man_ball_hawk_ints = ball_hawk_ints,
         man_PB = PB,
         man_ball_hawk_pbus = ball_hawk_pbus,
         man_FF = FF,
         man_penalties = penalties_count,
         man_penalties_eps = penalties_eps,
         `Man EPS` = eps_man_coverage) %>%
  dplyr::select(nflId_def, `Man EPS`, starts_with("man")) %>%
  full_join(zone_ratings %>%
              dplyr::select(nflId_def, eps_zone_coverage, zone_covers, accurate_targets, completions_allowed,
                            PB, ball_hawk_pbus, INT, ball_hawk_ints, T, FF, penalties_count,
                            penalties_eps) %>%
              rename(zone_accurate_targets = accurate_targets,
                     zone_completions_allowed = completions_allowed,
                     zone_INTs = INT,
                     zone_ball_hawk_ints = ball_hawk_ints,
                     zone_PB = PB,
                     zone_ball_hawk_pbus = ball_hawk_pbus,
                     zone_T = T,
                     zone_FF = FF,
                     zone_penalties = penalties_count,
                     zone_penalties_eps = penalties_eps,
                     `Zone EPS` = eps_zone_coverage) %>%
              dplyr::select(nflId_def, `Zone EPS`, starts_with("zone"))) %>%
  full_join(sos_ratings %>%
              dplyr::select(nflId_def, eps_tot_sos_adj, eps_zone_sos_adj, eps_man_sos_adj) %>%
              rename(`SOS EPS` = eps_tot_sos_adj,
                     `SOS Man EPS` = eps_man_sos_adj,
                     `SOS Zone EPS` = eps_zone_sos_adj))

overall_table[is.na(overall_table)] = 0

overall_table = overall_table %>%
  inner_join(player_info %>%
               rename(nflId_def = nflId)) %>%
  mutate(EPS = `Man EPS` + `Zone EPS`,
         `EPS Per Game` = EPS/G,
         `EPS Per Snap` = EPS/`Pass Snaps`,
         `SOS EPS Per Game` = `SOS EPS`/G,
         `SOS EPS Per Snap` = `SOS EPS`/`Pass Snaps`,
         `EPS Penalties` = man_penalties_eps + zone_penalties_eps,
         Covers = man_routes + zone_covers,
         `Accurate TAR` = man_accurate_targets + zone_accurate_targets,
         C = man_completions_allowed + zone_completions_allowed,
         INT = man_INTs + zone_INTs,
         `Ball Hawk INT` = man_ball_hawk_ints + zone_ball_hawk_ints,
         PB = man_PB + zone_PB,
         `Ball Hawk PB` = man_ball_hawk_pbus + zone_ball_hawk_pbus,
         T = man_T + zone_T,
         FF = man_FF + zone_FF,
         Penalties = man_penalties + zone_penalties,
         `Targeted C%` = replace_na(C/`Accurate TAR`, 0),
         `Hands on Ball % of Targets` = replace_na((INT + PB)/`Accurate TAR`, 0),
         `Hands on Ball % of Plays` = replace_na((INT + `Ball Hawk INT` +
                                         PB + `Ball Hawk PB`)/`Pass Snaps`, 0)) %>%
  inner_join(man_zone_perc) %>%
  dplyr::select(nflId_def, Pos, Player,  G, `Pass Snaps`, `%Man`,
                EPS, `Man EPS`, `Zone EPS`, `EPS Per Game`, `EPS Per Snap`,
                `SOS EPS`, `SOS Man EPS`, `SOS Zone EPS`, `SOS EPS Per Game`,
                `SOS EPS Per Snap`,
                Covers, `Accurate TAR`, C, `Targeted C%`,
                INT, `Ball Hawk INT`,
                PB, `Ball Hawk PB`, `Hands on Ball % of Targets`,
                `Hands on Ball % of Plays`, T, FF, Penalties) %>%
  arrange(desc(EPS)) %>%
  # dplyr::mutate_if(is.numeric, round, digits=4) %>%
  # mutate(`%Man` = scales::percent(`%Man`, accuracy = .1),
  #        `Targeted C%` = scales::percent(`Targeted C%`, accuracy = .1),
  #        `Hands on Ball % of Targets` = scales::percent(`Hands on Ball % of Targets`, accuracy = .1),
  #        `Hands on Ball % of Plays` = scales::percent(`Hands on Ball % of Plays`, accuracy = .1)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2)
  

write.csv(overall_table,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_overall.csv",
          row.names = FALSE)


# Man/Overall Aggregation --------------------------------------------------

man_overall_table = man_ratings %>%
  dplyr::select(nflId_def, eps_man_coverage, eps_tracking, eps_closing,
                eps_ball_skills, eps_tackling, eps_int_returns, eps_ball_hawk,
                routes, accurate_targets, completions_allowed,
                PB, ball_hawk_pbus, INT, ball_hawk_ints, T, FF, penalties_count,
                penalties_eps, man_tracking_win_rate) %>%
  rename(man_routes = routes,
         man_accurate_targets = accurate_targets,
         man_completions_allowed = completions_allowed,
         man_T = T,
         man_INTs = INT,
         man_ball_hawk_ints = ball_hawk_ints,
         man_PB = PB,
         man_ball_hawk_pbus = ball_hawk_pbus,
         man_FF = FF,
         man_penalties = penalties_count,
         man_penalties_eps = penalties_eps,
         `Man EPS` = eps_man_coverage,
         `Man EPS Tracking` = eps_tracking,
         `Man EPS Closing` = eps_closing,
         `Man EPS Ball Skills` = eps_ball_skills,
         `Man EPS Tackling` = eps_tackling,
         `Man EPS Ball Hawk` = eps_ball_hawk,
         `Man EPS INT Returns` = eps_int_returns,
         `Man Tracking Win Rate` = man_tracking_win_rate) %>%
  dplyr::select(nflId_def, `Man EPS`, starts_with("man")) %>%
  full_join(sos_ratings %>%
              rename(`SOS Man EPS` = eps_man_sos_adj,
                     `SOS Man EPS Tracking` = eps_man_tracking_sos_adj,
                     `SOS Man EPS Closing` = eps_man_closing_sos_adj,
                     `SOS Man EPS Ball Skills` = eps_man_ball_skills_sos_adj,
                     `SOS Man EPS Tackling` = eps_man_tackling_sos_adj))

man_overall_table[is.na(man_overall_table)] = 0

man_overall_table = man_overall_table %>%
  inner_join(player_info %>%
               rename(nflId_def = nflId)) %>%
  mutate(`EPS Penalties` = man_penalties_eps,
         Covers = man_routes,
         `Man EPS Per Game` = `Man EPS`/G,
         `Man EPS Per Cover` = `Man EPS`/Covers,
         `SOS Man EPS Per Game` = `SOS Man EPS`/G,
         `SOS Man EPS Per Cover` = `SOS Man EPS`/Covers,
         `Accurate TAR` = man_accurate_targets,
         C = man_completions_allowed,
         INT = man_INTs,
         `Ball Hawk INT` = man_ball_hawk_ints,
         PB = man_PB,
         `Ball Hawk PB` = man_ball_hawk_pbus,
         T = man_T,
         FF = man_FF,
         Penalties = man_penalties,
         `Targeted C%` = replace_na(C/`Accurate TAR`, 0),
         `Hands on Ball % of Targets` = replace_na((INT + PB)/`Accurate TAR`, 0),
         `Hands on Ball % of Plays` = replace_na((INT + `Ball Hawk INT` +
                                                    PB + `Ball Hawk PB`)/`Covers`, 0)) %>%
  inner_join(man_zone_perc) %>%
  dplyr::select(nflId_def, Pos, Player,  G, `Pass Snaps`, `%Man`,
                `Man EPS`,`Man EPS Per Game`, `Man EPS Per Cover`,
                `Man EPS Tracking`, `Man EPS Closing`,
                `Man EPS Ball Skills`, `Man EPS Tackling`, 
                `Man EPS Ball Hawk`, `Man EPS INT Returns`,
                `SOS Man EPS`,`SOS Man EPS Per Game`,`SOS Man EPS Per Cover`,
                `SOS Man EPS Tracking`, `SOS Man EPS Closing`,
                `SOS Man EPS Ball Skills`, `SOS Man EPS Tackling`,
                Covers, `Accurate TAR`, C, `Targeted C%`,
                INT, `Ball Hawk INT`, PB, `Ball Hawk PB`, 
                `Hands on Ball % of Targets`,
                `Hands on Ball % of Plays`,
                T, FF, Penalties, `Man Tracking Win Rate`) %>%
  arrange(desc(`Man EPS`)) %>%
  # dplyr::mutate_if(is.numeric, round, digits=4) %>%
  # mutate(`%Man` = scales::percent(`%Man`, accuracy = .1),
  #        `Targeted C%` = scales::percent(`Targeted C%`, accuracy = .1),
  #        `Hands on Ball % of Targets` = scales::percent(`Hands on Ball % of Targets`, accuracy = .1),
  #        `Hands on Ball % of Plays` = scales::percent(`Hands on Ball % of Plays`, accuracy = .1),
  #        `Man Tracking Win Rate` = scales::percent(`Man Tracking Win Rate`, accuracy = .1)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2)

write.csv(man_overall_table,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_man.csv",
          row.names = FALSE)


# Zone Overall Table ------------------------------------------------------

zone_overall_table = zone_ratings %>%
  dplyr::select(nflId_def, eps_zone_coverage, eps_closing,
                eps_ball_skills, eps_tackling, eps_int_returns, eps_ball_hawk,
                zone_covers, accurate_targets, completions_allowed,
                PB, ball_hawk_pbus, INT, ball_hawk_ints, T, FF, penalties_count,
                penalties_eps) %>%
  rename(zone_covers = zone_covers,
         zone_accurate_targets = accurate_targets,
         zone_completions_allowed = completions_allowed,
         zone_T = T,
         zone_INTs = INT,
         zone_ball_hawk_ints = ball_hawk_ints,
         zone_PB = PB,
         zone_ball_hawk_pbus = ball_hawk_pbus,
         zone_FF = FF,
         zone_penalties = penalties_count,
         zone_penalties_eps = penalties_eps,
         `Zone EPS` = eps_zone_coverage,
         `Zone EPS Closing` = eps_closing,
         `Zone EPS Ball Skills` = eps_ball_skills,
         `Zone EPS Tackling` = eps_tackling,
         `Zone EPS Ball Hawk` = eps_ball_hawk,
         `Zone EPS INT Returns` = eps_int_returns) %>%
  dplyr::select(nflId_def, `Zone EPS`, starts_with("zone")) %>%
  full_join(sos_ratings %>%
              rename(`SOS Zone EPS` = eps_zone_sos_adj,
                     `SOS Zone EPS Closing` = eps_zone_closing_sos_adj,
                     `SOS Zone EPS Ball Skills` = eps_zone_ball_skills_sos_adj,
                     `SOS Zone EPS Tackling` = eps_zone_tackling_sos_adj))

zone_overall_table[is.na(zone_overall_table)] = 0

zone_overall_table = zone_overall_table %>%
  inner_join(player_info %>%
               rename(nflId_def = nflId)) %>%
  mutate(`EPS Penalties` = zone_penalties_eps,
         `Zone EPS Per Game` = `Zone EPS`/G,
         `Zone EPS Per Snap` = `Zone EPS`/`Pass Snaps`,
         `SOS Zone EPS Per Game` = `SOS Zone EPS`/G,
         `SOS Zone EPS Per Snap` = `SOS Zone EPS`/`Pass Snaps`,
         Covers = zone_covers,
         `Accurate TAR` = zone_accurate_targets,
         C = zone_completions_allowed,
         INT = zone_INTs,
         `Ball Hawk INT` = zone_ball_hawk_ints,
         PB = zone_PB,
         `Ball Hawk PB` = zone_ball_hawk_pbus,
         T = zone_T,
         FF = zone_FF,
         Penalties = zone_penalties,
         `Targeted C%` = replace_na(C/`Accurate TAR`, 0),
         `Hands on Ball % of Targets` = replace_na((INT + PB)/`Accurate TAR`, 0),
         `Hands on Ball % of Plays` = replace_na((INT + `Ball Hawk INT` +
                                                    PB + `Ball Hawk PB`)/`Covers`, 0)) %>%
  inner_join(man_zone_perc) %>%
  mutate(`%Zone` = 1 - `%Man`) %>%
  dplyr::select(nflId_def, Pos, Player,  G, `Pass Snaps`, `%Zone`,
                `Zone EPS`, `Zone EPS Per Game`, `Zone EPS Per Snap`,
                `Zone EPS Closing`,
                `Zone EPS Ball Skills`, `Zone EPS Tackling`, 
                `Zone EPS Ball Hawk`, `Zone EPS INT Returns`,
                `SOS Zone EPS`, `SOS Zone EPS Per Game`,`SOS Zone EPS Per Snap`,
                `SOS Zone EPS Closing`,
                `SOS Zone EPS Ball Skills`, `SOS Zone EPS Tackling`,
                Covers, `Accurate TAR`, C, `Targeted C%`,,
                INT, `Ball Hawk INT`, PB, `Ball Hawk PB`, 
                `Hands on Ball % of Targets`,
                `Hands on Ball % of Plays`,
                T, FF, Penalties) %>%
  arrange(desc(`Zone EPS`)) %>%
  # dplyr::mutate_if(is.numeric, round, digits=4) %>%
  # mutate(`%Zone` = scales::percent(`%Zone`, accuracy = .1),
  #        `Targeted C%` = scales::percent(`Targeted C%`, accuracy = .1),
  #        `Hands on Ball % of Targets` = scales::percent(`Hands on Ball % of Targets`, accuracy = .1),
  #        `Hands on Ball % of Plays` = scales::percent(`Hands on Ball % of Plays`, accuracy = .1)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2)

write.csv(zone_overall_table,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_zone.csv",
          row.names = FALSE)


# by route
route_man_ratings = read.csv("~/Desktop/CoverageNet/src/04_evaluate_players/outputs/overall_player_skills_summary_by_route.csv")
route_zone_ratings = read.csv("~/Desktop/CoverageNet/src/05_evaluate_players_zone/outputs/overall_player_skills_summary_by_route.csv")

route_overall_table = route_man_ratings %>%
  dplyr::select(nflId_def, route, eps_man_coverage, routes, accurate_targets, completions_allowed,
                PB, INT, T, FF) %>%
  rename(man_routes = routes,
         man_accurate_targets = accurate_targets,
         man_completions_allowed = completions_allowed,
         man_T = T,
         man_INTs = INT,
         man_PB = PB,
         man_FF = FF,
         `Man EPS` = eps_man_coverage) %>%
  dplyr::select(nflId_def, route, `Man EPS`, starts_with("man")) %>%
  full_join(route_zone_ratings %>%
              dplyr::select(nflId_def, route, eps_zone_coverage, 
                            zone_covers, accurate_targets, completions_allowed,
                            PB, INT, T, FF) %>%
              rename(zone_accurate_targets = accurate_targets,
                     zone_completions_allowed = completions_allowed,
                     zone_INTs = INT,
                     zone_PB = PB,
                     zone_T = T,
                     zone_FF = FF,
                     `Zone EPS` = eps_zone_coverage) %>%
              dplyr::select(nflId_def, route, `Zone EPS`, starts_with("zone")))

route_overall_table[is.na(route_overall_table)] = 0

route_overall_table = route_overall_table %>%
  mutate(EPS = `Man EPS` + `Zone EPS`,
         Covers = man_routes + zone_covers,
         `Accurate TAR` = man_accurate_targets + zone_accurate_targets,
         C = man_completions_allowed + zone_completions_allowed,
         INT = man_INTs + zone_INTs,
         PB = man_PB + zone_PB,
         T = man_T + zone_T,
         FF = man_FF + zone_FF) %>%
  inner_join(player_info %>%
               rename(nflId_def = nflId)) %>%
  inner_join(man_zone_perc) %>%
  dplyr::select(nflId_def, Pos, Player,  G, `Pass Snaps`, route, `%Man`,
                EPS, `Man EPS`, `Zone EPS`,
                Covers, `Accurate TAR`, C, INT,
                PB, T, FF) %>%
  arrange(desc(EPS)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2)

write.csv(route_overall_table,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_route_overall.csv",
          row.names = FALSE)

# Man/Overall Aggregation --------------------------------------------------

route_man_overall_table = route_man_ratings %>%
  dplyr::select(nflId_def, route, eps_man_coverage, eps_tracking, eps_closing,
                eps_ball_skills, eps_tackling,
                routes, accurate_targets, completions_allowed,
                PB, INT, T, FF) %>%
  rename(man_routes = routes,
         man_accurate_targets = accurate_targets,
         man_completions_allowed = completions_allowed,
         man_T = T,
         man_INTs = INT,
         man_PB = PB,
         man_FF = FF,
         `Man EPS` = eps_man_coverage,
         `Man EPS Tracking` = eps_tracking,
         `Man EPS Closing` = eps_closing,
         `Man EPS Ball Skills` = eps_ball_skills,
         `Man EPS Tackling` = eps_tackling) %>%
  dplyr::select(nflId_def, route, `Man EPS`, starts_with("man"))

route_man_overall_table[is.na(route_man_overall_table)] = 0

route_man_overall_table = route_man_overall_table %>%
  mutate(Covers = man_routes,
         `Accurate TAR` = man_accurate_targets,
         C = man_completions_allowed,
         INT = man_INTs,
         PB = man_PB,
         T = man_T,
         FF = man_FF) %>%
  inner_join(player_info %>%
               rename(nflId_def = nflId)) %>%
  inner_join(man_zone_perc) %>%
  dplyr::select(nflId_def, Pos, Player,  G, `Pass Snaps`, `%Man`,route,
                `Man EPS`,`Man EPS Tracking`, `Man EPS Closing`,
                `Man EPS Ball Skills`, `Man EPS Tackling`, 
                Covers, `Accurate TAR`, C, INT,
                PB, T, FF) %>%
  arrange(desc(`Man EPS`)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2)

write.csv(route_man_overall_table,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_route_man.csv",
          row.names = FALSE)


# Zone Overall Table ------------------------------------------------------

route_zone_overall_table = route_zone_ratings %>%
  dplyr::select(nflId_def, route, eps_zone_coverage, eps_closing,
                eps_ball_skills, eps_tackling,
                zone_covers, accurate_targets, completions_allowed,
                PB, INT, T, FF) %>%
  rename(zone_covers = zone_covers,
         zone_accurate_targets = accurate_targets,
         zone_completions_allowed = completions_allowed,
         zone_T = T,
         zone_INTs = INT,
         zone_PB = PB,
         zone_FF = FF,
         `Zone EPS` = eps_zone_coverage,
         `Zone EPS Closing` = eps_closing,
         `Zone EPS Ball Skills` = eps_ball_skills,
         `Zone EPS Tackling` = eps_tackling) %>%
  dplyr::select(nflId_def, route, `Zone EPS`, starts_with("zone"))

route_zone_overall_table[is.na(route_zone_overall_table)] = 0

route_zone_overall_table = route_zone_overall_table %>%
  mutate(Covers = zone_covers,
         `Accurate TAR` = zone_accurate_targets,
         C = zone_completions_allowed,
         INT = zone_INTs,
         PB = zone_PB,
         T = zone_T,
         FF = zone_FF) %>%
  inner_join(player_info %>%
               rename(nflId_def = nflId)) %>%
  inner_join(man_zone_perc) %>%
  mutate(`%Zone` = 1 - `%Man`) %>%
  dplyr::select(nflId_def, Pos, Player,  G, `Pass Snaps`, `%Zone`, route,
                `Zone EPS`, `Zone EPS Closing`,
                `Zone EPS Ball Skills`, `Zone EPS Tackling`, 
                Covers, `Accurate TAR`, C, INT,
                PB, T, FF) %>%
  arrange(desc(`Zone EPS`)) %>%
  dplyr::mutate_if(is.numeric, round, digits=2)

write.csv(route_zone_overall_table,
          "~/Desktop/CoverageNet/src/07_dashboard_aggs/outputs/player_ratings/player_ratings_route_zone.csv",
          row.names = FALSE)



