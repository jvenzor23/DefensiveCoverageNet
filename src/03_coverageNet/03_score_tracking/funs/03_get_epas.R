get_epas <- function(pass_attempt_class_preds,
                     pass_attempt_caught_preds,
                     pass_attempt_intercepted_preds,
                     pbp_data_2018
                     ) {

  # Compute EPA_throw For All Completions Using NNET Model ------------------
  
  pass_attempt_caught_plays = pass_attempt_caught_preds %>%
    inner_join(pbp_data_2018 %>%
                 inner_join(my_epa_data) %>%
                 select(gameId, playId, TimeSecs_Remaining,
                        down, ydstogo, yrdline100,
                        air_yards, my_ep))
  
  # fixing safeties and td's, and adjusting probabilities
  pass_attempt_caught_plays2 = pass_attempt_caught_plays %>%
    mutate(yardline_end = yrdline100 - offensePlayResult) %>%
    mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                    yardline_end < 0 ~ 0,
                                    TRUE ~ as.numeric(yardline_end))) %>%
    select(-offensePlayResult) %>%
    group_by_at(vars(-probability)) %>%
    summarize(probability = sum(probability))
  
  # fixing the down/distance/posteam
  pass_attempt_caught_plays3 = pass_attempt_caught_plays2 %>%
    ungroup() %>%
    mutate(down = as.integer(down)) %>%
    mutate(first_down_flag = if_else(yrdline100 - yardline_end >= ydstogo, 1, 0),
           turnover_on_downs = if_else((first_down_flag == 0)&(down == 4), 1, 0),
           down_new = if_else(turnover_on_downs|first_down_flag, 1, down + 1),
           offensePlayResult = yrdline100 - yardline_end,
           yardline_new = if_else(turnover_on_downs == 1, 100 - yardline_end, yardline_end),
           ydstogo_new = if_else(turnover_on_downs|first_down_flag, 
                                 pmin(yardline_new, 10),
                                 ydstogo - (yrdline100 - yardline_new)),
    ) %>%
    inner_join(pbp_data_2018 %>%
                 group_by(gameId, game_half) %>%
                 mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
                 select(gameId, playId, TimeSecs_Remaining_new)
    ) %>%
    mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
    select(gameId, playId, targetNflId, frameId, turnover_on_downs, offensePlayResult,
           ends_with("_new"), my_ep, probability)
  
  colnames(pass_attempt_caught_plays3) = gsub("_new", "", names(pass_attempt_caught_plays3))
  
  pass_attempt_caught_plays3 = pass_attempt_caught_plays3 %>%
    rename(yrdline100 = yardline) %>%
    mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
    mutate(log_ydstogo = log(ydstogo),
           Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
           
           # Changing down into a factor variable: 
           down = factor(down)) %>%
    mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))
  
  rm(pass_attempt_caught_plays)
  rm(pass_attempt_caught_plays2)
  
  pass_attempt_caught_epa = as.data.frame(predict(ep_model, pass_attempt_caught_plays3, type = "probs")) %>%
    mutate(my_ep_pass_attempt = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
           + 2*(Opp_Safety - Safety))
  
  pass_attempt_caught_plays3$my_ep_pass_attempt = pass_attempt_caught_epa$my_ep_pass_attempt
  
  pass_attempt_caught_plays4 = pass_attempt_caught_plays3 %>%
    mutate(my_ep_pass_attempt = case_when(yrdline100 == 0 ~ 7,
                                          yrdline100 == 100 ~ -2,
                                          TRUE ~ my_ep_pass_attempt)) %>%
    mutate(my_ep_pass_attempt = if_else(turnover_on_downs == 1, -my_ep_pass_attempt, my_ep_pass_attempt)) %>%
    mutate(my_ep_pass_attempt = my_ep_pass_attempt - my_ep)
  
  pass_attempt_caught_plays5 = pass_attempt_caught_plays4 %>%
    group_by(gameId, playId, targetNflId, frameId) %>%
    summarize(ey_catch = sum(offensePlayResult*probability),
              epa_pass_attempt = sum(my_ep_pass_attempt*probability))
  
  rm(pass_attempt_caught_plays3,
     pass_attempt_caught_plays4,
     pass_attempt_caught_preds,
     pass_attempt_caught_epa)
  
  # Compute EPA_throw For All Incompletions Using NNET Model ----------------
  
  pass_attempt_incomplete_plays = pass_attempt_class_preds %>%
    select(gameId, playId, targetNflId, frameId) %>%
    mutate(offensePlayResult = 0,
           probability = 1) %>%
    inner_join(pbp_data_2018 %>%
                 inner_join(my_epa_data) %>%
                 select(gameId, playId, TimeSecs_Remaining,
                        down, ydstogo, yrdline100,
                        air_yards, my_ep))
  
  # fixing safeties and td's, and adjusting probabilities
  pass_attempt_incomplete_plays2 = pass_attempt_incomplete_plays %>%
    mutate(yardline_end = yrdline100 - offensePlayResult) %>%
    mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                    yardline_end < 0 ~ 0,
                                    TRUE ~ as.numeric(yardline_end))) %>%
    select(-offensePlayResult) %>%
    group_by_at(vars(-probability)) %>%
    summarize(probability = sum(probability))
  
  # fixing the down/distance/posteam
  pass_attempt_incomplete_plays3 = pass_attempt_incomplete_plays2 %>%
    ungroup() %>%
    mutate(down = as.integer(down)) %>%
    mutate(first_down_flag = if_else(yrdline100 - yardline_end >= ydstogo, 1, 0),
           turnover_on_downs = if_else((first_down_flag == 0)&(down == 4), 1, 0),
           down_new = if_else(turnover_on_downs|first_down_flag, 1, down + 1),
           offensePlayResult = yrdline100 - yardline_end,
           yardline_new = if_else(turnover_on_downs == 1, 100 - yardline_end, yardline_end),
           ydstogo_new = if_else(turnover_on_downs|first_down_flag, 
                                 pmin(yardline_new, 10),
                                 ydstogo - (yrdline100 - yardline_new)),
    ) %>%
    inner_join(pbp_data_2018 %>%
                 group_by(gameId, game_half) %>%
                 mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
                 select(gameId, playId, TimeSecs_Remaining_new)
    ) %>%
    mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
    select(gameId, playId, targetNflId, frameId, turnover_on_downs, offensePlayResult,
           ends_with("_new"), my_ep, probability)
  
  colnames(pass_attempt_incomplete_plays3) = gsub("_new", "", names(pass_attempt_incomplete_plays3))
  
  pass_attempt_incomplete_plays3 = pass_attempt_incomplete_plays3 %>%
    rename(yrdline100 = yardline) %>%
    mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
    mutate(log_ydstogo = log(ydstogo),
           Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
           
           # Changing down into a factor variable: 
           down = factor(down)) %>%
    mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))
  
  rm(pass_attempt_incomplete_plays)
  rm(pass_attempt_incomplete_plays2)
  
  
  pass_attempt_incomplete_epa = as.data.frame(predict(ep_model, pass_attempt_incomplete_plays3, type = "probs")) %>%
    mutate(my_ep_pass_attempt = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
           + 2*(Opp_Safety - Safety))
  
  pass_attempt_incomplete_plays3$my_ep_pass_attempt = pass_attempt_incomplete_epa$my_ep_pass_attempt
  
  pass_attempt_incomplete_plays4 = pass_attempt_incomplete_plays3 %>%
    mutate(my_ep_pass_attempt = case_when(yrdline100 == 0 ~ 7,
                                          yrdline100 == 100 ~ -2,
                                          TRUE ~ my_ep_pass_attempt)) %>%
    mutate(my_ep_pass_attempt = if_else(turnover_on_downs == 1, -my_ep_pass_attempt, my_ep_pass_attempt)) %>%
    mutate(my_ep_pass_attempt = my_ep_pass_attempt - my_ep)
  
  pass_attempt_incomplete_plays5 = pass_attempt_incomplete_plays4 %>%
    group_by(gameId, playId, targetNflId, frameId) %>%
    summarize(epa_pass_incomplete = sum(my_ep_pass_attempt*probability))
  
  rm(pass_attempt_incomplete_plays3,
     pass_attempt_incomplete_plays4,
     pass_attempt_incomplete_epa)
  
  # Compute EPA_throw For All Interceptions Using NNET Model ----------------
  
  pass_attempt_intercepted_plays = pass_attempt_intercepted_preds %>%
    inner_join(pbp_data_2018 %>%
                 inner_join(my_epa_data) %>%
                 select(gameId, playId, TimeSecs_Remaining,
                        down, ydstogo, yrdline100,
                        air_yards, my_ep))
  
  # fixing safeties and td's, and adjusting probabilities
  pass_attempt_intercepted_plays2 = pass_attempt_intercepted_plays %>%
    mutate(yardline_end = (100 - (yrdline100 - offensePlayResult))) %>%
    mutate(yardline_end = case_when(yardline_end > 100 ~ 100,
                                    yardline_end < 0 ~ 0,
                                    TRUE ~ as.numeric(yardline_end))) %>%
    select(-offensePlayResult) %>%
    group_by_at(vars(-probability)) %>%
    summarize(probability = sum(probability))
  
  # fixing the down/distance/posteam
  pass_attempt_intercepted_plays3 = pass_attempt_intercepted_plays2 %>%
    ungroup() %>%
    mutate(down = as.integer(down)) %>%
    mutate(turnover = 1,
           down_new = 1,
           offensePlayResult = yardline_end + yrdline100 - 100,
           yardline_new = yardline_end,
           ydstogo_new = pmin(yardline_new, 10)
    ) %>%
    inner_join(pbp_data_2018 %>%
                 group_by(gameId, game_half) %>%
                 mutate(TimeSecs_Remaining_new = lead(TimeSecs_Remaining)) %>%
                 select(gameId, playId, TimeSecs_Remaining_new)
    ) %>%
    mutate(TimeSecs_Remaining_new = replace_na(TimeSecs_Remaining_new, 0)) %>%
    select(gameId, playId, targetNflId, frameId, turnover, offensePlayResult,
           ends_with("_new"), my_ep, probability)
  
  colnames(pass_attempt_intercepted_plays3) = gsub("_new", "", names(pass_attempt_intercepted_plays3))
  
  pass_attempt_intercepted_plays3 = pass_attempt_intercepted_plays3 %>%
    rename(yrdline100 = yardline) %>%
    mutate(GoalToGo = if_else(ydstogo == yrdline100, 1, 0)) %>%
    mutate(log_ydstogo = log(ydstogo),
           Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
           
           # Changing down into a factor variable: 
           down = factor(down)) %>%
    mutate(log_ydstogo = if_else(is.finite(log_ydstogo), log_ydstogo, 0))
  
  rm(pass_attempt_intercepted_plays,
     pass_attempt_intercepted_plays2)
  
  pass_attempt_intercepted_epa = as.data.frame(predict(ep_model, pass_attempt_intercepted_plays3, type = "probs")) %>%
    mutate(my_ep_int = 6*(Touchdown - Opp_Touchdown) + 3*(Field_Goal - Opp_Field_Goal)
           + 2*(Opp_Safety - Safety))
  
  pass_attempt_intercepted_plays3$my_ep_int = pass_attempt_intercepted_epa$my_ep_int
  
  pass_attempt_intercepted_plays4 = pass_attempt_intercepted_plays3 %>%
    mutate(my_ep_int = case_when(yrdline100 == 0 ~ 7,
                                 yrdline100 == 100 ~ -2,
                                 TRUE ~ my_ep_int)) %>%
    mutate(my_ep_int = if_else(turnover == 1, -my_ep_int, my_ep_int)) %>%
    mutate(my_epa_throw = my_ep_int - my_ep)
  
  pass_attempt_intercepted_plays5 = pass_attempt_intercepted_plays4 %>%
    group_by(gameId, playId, targetNflId, frameId) %>%
    summarize(ey_int = sum(offensePlayResult*probability),
              epa_int = sum(my_ep_int*probability))
  
  rm(pass_attempt_intercepted_plays3,
     pass_attempt_intercepted_plays4,
     pass_attempt_intercepted_preds,
     pass_attempt_intercepted_epa)
  
  # Saving Metrics -------------------------------------------------------
  
  plays_with_my_epa4 = pass_attempt_class_preds %>%
    inner_join(pass_attempt_caught_plays5) %>%
    inner_join(pass_attempt_incomplete_plays5) %>%
    inner_join(pass_attempt_intercepted_plays5)
  
  plays_with_my_epa5 = plays_with_my_epa4 %>%
    rename(epa_pass_caught = epa_pass_attempt) %>%
    mutate(epa_pass_attempt = epa_pass_caught*C_prob +
             epa_pass_incomplete*I_prob +
             epa_int*IN_prob)
  
  plays_with_my_epa6 = plays_with_my_epa5 %>%
    filter(!is.na(epa_pass_attempt)) %>%
    select(gameId, playId, targetNflId, frameId,C_prob, I_prob, IN_prob, epa_pass_attempt)

  return(plays_with_my_epa6)
  
}