get_tensor <- function(pass_attempts) {

  # finding plays without dir
  plays_remove = pass_attempts %>%
    filter(!is.na(nflId)) %>%
    filter(is.na(dir)) %>%
    distinct(gameId, playId)
  
  plays_remove2 = pass_attempts %>%
    ungroup() %>%
    group_by(gameId, playId, targetNflId, frameId) %>%
    summarize(receivers = sum(IsReceiver)) %>%
    filter(receivers != 1)
  
  # need to remove duplicates of plays with no football!
  plays_remove3 = pass_attempts %>%
    group_by(gameId, playId, targetNflId, frameId) %>%
    filter(sum(is.na(nflId)) == 0) %>%
    distinct(gameId, playId, targetNflId, frameId)
  
  pass_attempts = pass_attempts %>%
    anti_join(plays_remove) %>%
    anti_join(plays_remove2) %>%
    anti_join(plays_remove3)
  
  pass_attempts_tot = pass_attempts %>%
    mutate(group = "full")
  
  # Deriving Features for Neural Network ------------------------------------
  
  # Features:
  # ------------------ FROM KAGGLE WINNERS ------------------------
  # def S(x,y)
  # receiver S(x,y)
  # def (x,y) - receiver (x,y)
  # def S(x,y) - receiver S(x,y)
  # def (x,y) - football (x,y)
  # off (x,y) - def (x,y)
  # off S(x,y) - def S(x,y)
  # distance of WR down the field
  # football speed (?)
  # ---------------------------------------------------------------
  
  # computing S(x,y)
  pass_attempts_tot2 = pass_attempts_tot %>%
    mutate(s_x = s*cos((90-dir)*pi/180),
           s_y = s*sin((90-dir)*pi/180)) %>%
    select(group, gameId, playId, targetNflId, frameId,
           nflId, x, y, s, s_x, s_y, IsOnOffense, IsReceiver, 
           YardsFromOwnGoal)
  
  # ball carrier run data
  targeted_wr_data = pass_attempts_tot2 %>%
    filter(IsReceiver) %>%
    arrange(gameId, playId, targetNflId, frameId) %>%
    rename(receiver_x = x,
           receiver_y = y,
           receiver_s_x = s_x,
           receiver_s_y = s_y) %>%
    select(-IsOnOffense, -IsReceiver, -s) %>%
    mutate(receiver_distance_from_los = receiver_x - YardsFromOwnGoal)
  
  offense_data = pass_attempts_tot2 %>%
    filter(IsOnOffense & (!IsReceiver)) %>%
    arrange(gameId, playId, targetNflId, frameId, nflId) %>%
    rename(offense_x = x,
           offense_y = y,
           offense_s_x = s_x,
           offense_s_y = s_y,
           offense_nflId = nflId) %>%
    select(-IsOnOffense, -IsReceiver, -s)
  
  defense_data = pass_attempts_tot2 %>%
    filter(!IsOnOffense & (!is.na(nflId))) %>%
    arrange(gameId, playId, targetNflId, frameId, nflId) %>%
    rename(defense_x = x,
           defense_y = y,
           defense_s_x = s_x,
           defense_s_y = s_y,
           defense_nflId = nflId) %>%
    select(-IsOnOffense, -IsReceiver, -s)
  
  football_data = pass_attempts_tot2 %>%
    filter(is.na(nflId)) %>%
    arrange(gameId, playId, targetNflId, frameId, nflId) %>%
    rename(football_x = x,
           football_y = y,
           football_s = s) %>%
    select(gameId, playId, starts_with("football"))
  
  # getting defense vs. rusher stats
  tot_data1 = defense_data %>%
    inner_join(targeted_wr_data %>% select(-nflId, -YardsFromOwnGoal), by = c("gameId", "playId", "targetNflId", "frameId")) %>%
    inner_join(football_data, by = c("gameId", "playId", "targetNflId", "frameId")) %>%
    mutate(defense_receiver_x = defense_x - receiver_x,
           defense_receiver_y = defense_y - receiver_y,
           defense_receiver_s_x = defense_s_x - receiver_s_x,
           defense_receiver_s_y = defense_s_y - receiver_s_y,
           defense_football_x = defense_x - football_x,
           defense_football_y = defense_y - football_y,
           receiver_football_x = receiver_x - football_x,
           receiver_football_y = receiver_y - football_y,
           defender_receiver_football_dist_diff = sqrt((defense_x - football_x)^2 + (defense_y - football_y)^2) - 
             sqrt((receiver_x - football_x)^2 + (receiver_y - football_y)^2),
           receiver_football_defender_dist_diff = sqrt((receiver_x - football_x)^2 + (receiver_y - football_y)^2) - 
             sqrt((receiver_x - defense_x)^2 + (receiver_y - defense_y)^2)
    ) %>%
    select(-starts_with("receiver"), receiver_s_x, receiver_s_y, 
           receiver_distance_from_los, receiver_football_x, receiver_football_y,
           receiver_football_defender_dist_diff, -football_x, -football_y)
  
  # getting defense vs. offense stats
  tot_data2 = tot_data1 %>%
    inner_join(offense_data, by = c("gameId", "playId", "targetNflId", "frameId")) %>%
    mutate(defense_offense_x = defense_x - offense_x,
           defense_offense_y = defense_y - offense_y,
           defense_offense_s_x = defense_s_x - offense_s_x,
           defense_offense_s_y = defense_s_y - offense_s_y
    ) %>%
    arrange(gameId, playId, targetNflId, frameId, defense_nflId, offense_nflId) %>%
    select(group, gameId, playId, targetNflId, frameId,
           defense_nflId, offense_nflId, 
           defense_s_x, defense_s_y,
           defense_receiver_x, defense_receiver_y,
           defense_receiver_s_x, defense_receiver_s_y,
           defense_offense_x, defense_offense_y,
           defense_offense_s_x, defense_offense_s_y,
           receiver_s_x, receiver_s_y, receiver_distance_from_los,
           defense_football_x, defense_football_y, receiver_football_x,
           receiver_football_y, football_s,
           receiver_football_defender_dist_diff,
           defender_receiver_football_dist_diff)
  
  # adjusting nflIds to 1-10/1-11
  tot_data3 = tot_data2 %>%
    group_by(group, gameId, playId, targetNflId, frameId) %>%
    mutate(defenseId = dense_rank(defense_nflId),
           offenseId = dense_rank(offense_nflId)) %>%
    ungroup() 
  
  numbered_plays = tot_data3 %>%
    distinct(group, gameId, playId, targetNflId, frameId) %>%
    arrange(group, gameId, playId, targetNflId, frameId) %>%
    group_by(group) %>%
    mutate(playId2 = row_number())
  
  tot_data3 = tot_data3 %>%
    inner_join(numbered_plays) %>%
    arrange(group, gameId, playId2, targetNflId, frameId, defense_nflId, offense_nflId) %>%
    select(group, gameId, playId, targetNflId, frameId, defense_nflId, offense_nflId, playId2, defenseId, offenseId, everything())
  
  # Adding in Padding to Each Play ------------------------------------------
  
  max_def_players = 11
  max_off_players = 10
  
  n_off_df = data.frame("offenseId" = seq(1, max_off_players), "dummy" = 1)
  n_def_df = data.frame("defenseId" = seq(1, max_def_players), "dummy" = 1)
  
  padding_df = n_off_df %>%
    full_join(n_def_df) %>%
    full_join(tot_data3 %>%
                distinct(group, gameId, playId, targetNflId, frameId, playId2) %>%
                mutate(dummy = 1)) %>%
    select(group, gameId, playId, defenseId, offenseId, targetNflId, frameId, playId2) %>%
    arrange(group, gameId, playId, defenseId, offenseId, targetNflId, frameId, playId2)
  
  tot_data4 = padding_df %>%
    left_join(tot_data3) %>%
    arrange(group, gameId, playId2, defenseId, offenseId) %>%
    ungroup() %>%
    select(group, gameId, playId, targetNflId, frameId, defense_nflId, offense_nflId, playId2, defenseId, offenseId, everything())
  
  tot_data4[is.na(tot_data4)] = 0
  
  return(tot_data4)
  
}
