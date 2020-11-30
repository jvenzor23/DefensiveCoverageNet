get_tracking_pt_data <- function(pt_data, 
                                 plays) {

  plays_start = pt_data %>%
    filter(event == "ball_snap") %>%
    distinct(gameId, playId, frameId) %>%
    select(gameId, playId, frameId) %>%
    arrange(gameId, playId) %>%
    rename(frameId_start = frameId) %>%
    mutate(frameId_start = frameId_start + 5)
  
  plays_end_with_pass = pt_data %>%
    filter(event == "pass_forward") %>%
    group_by(gameId, playId) %>%
    summarize(frameId_end = min(frameId)) %>%
    arrange(gameId, playId)
  
  plays_with_sack = pt_data %>%
    inner_join(plays %>%
                 filter(passResult == 'S') %>%
                 distinct(gameId, playId)) %>%
    filter(event %in% c('first_contact', 'qb_sack', 'qb_strip_sack', 'tackle')) %>%
    group_by(gameId, playId) %>%
    summarize(frameId_end = min(frameId))
  
  plays_end = rbind(plays_end_with_pass,
                    plays_with_sack) %>%
    distinct(gameId, playId, .keep_all = TRUE)
  
  plays_clip = plays_start %>% inner_join(plays_end)
  
  # Filtering the player tracking data to our times of interest -------------
  
  pt_data2 = pt_data %>%
    inner_join(plays_clip) %>%
    filter(frameId >= frameId_start,
           frameId <= frameId_end)
  
  # Identifying Players Who Ran Routes As Potential Targets -----------------
  
  possible_targets = rbind(
    pt_data2 %>%
      inner_join(plays %>% 
                   filter(passResult != 'S') %>%
                   select(gameId, playId)) %>%
      filter(route != "") %>%
      distinct(gameId, playId, nflId) %>%
      select(gameId, playId, nflId) %>%
      arrange(gameId, playId) %>%
      rename(targetNflId = nflId),
    pt_data2 %>%
      inner_join(plays %>% 
                   filter(passResult == 'S') %>%
                   select(gameId, playId)) %>%
      filter(IsOnOffense,
             position != 'QB') %>%
      distinct(gameId, playId, nflId) %>%
      select(gameId, playId, nflId) %>%
      arrange(gameId, playId) %>%
      rename(targetNflId = nflId)) %>%
    arrange(gameId, playId)
  
  
  # Adding All Possible Targets to Each Play --------------------------------
  
  pass_attempts = pt_data2 %>%
    inner_join(possible_targets) %>%
    arrange(gameId, playId, targetNflId, frameId) %>%
    select(-frameId_start, -frameId_end) %>%
    mutate(IsReceiver = targetNflId == nflId) %>%
    select(-time) %>%
    group_by(gameId, playId, frameId, targetNflId) %>%
    # ensuring exactly one player listed as targeted receiver
    filter(sum(1*IsReceiver, na.rm = TRUE) == 1) %>%
    select(-playDirection, -route, -jerseyNumber, -displayName)
  
  return(pass_attempts)
  
}