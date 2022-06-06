
# Function takes in Play by play dataset from nflfastR::load_pbp() and tidys --> adds win percentage
get_win_pct <- function(season_pbp) {

  season_year <-  season_pbp$season[1]
  
  logger::log_info("Calculating {season_year} win loss records...")
  
  # Slice off top row for each game to extract winners/loser
  season_pbp <-
    season_pbp %>% 
    dplyr::select(season, game_id, home_team, away_team, season_type, week, home_score, away_score, result, roof, surface) %>% 
    dplyr::group_by(game_id) %>%
    slice(1) 
  
  # Create Win/loss/tie columns
  team_results <- 
    season_pbp %>% 
    tidyr::pivot_longer(
      cols      = c(away_team, home_team),
      names_to  = "home_away",
      values_to = "team"
    ) %>% 
    dplyr::mutate(
      win = case_when(
        home_away == "away_team" & away_score > home_score ~ 1,
        home_away == "away_team" & away_score < home_score ~ 0,
        home_away == "home_team" & home_score > away_score ~ 1,
        home_away == "home_team" & home_score < away_score ~ 0,
        away_score == home_score                           ~ 0
      ),
      loss = case_when(
        home_away == "away_team" & away_score > home_score ~ 0,
        home_away == "away_team" & away_score < home_score ~ 1,
        home_away == "home_team" & home_score > away_score ~ 0,
        home_away == "home_team" & home_score < away_score ~ 1,
        away_score == home_score                           ~ 0
      ),
      tie = case_when(
        away_score == home_score                           ~ 0.5,
        TRUE                                               ~ 0
        )
      ) %>% 
    tidyr::replace_na(list(win = 0, loss = 0, tie = 0))
  
  # Take cumalitve sum of wins to calc win percentage
  team_results <- 
    team_results %>% 
    dplyr::group_by(team) %>% 
    dplyr::arrange(week) %>% 
    dplyr::mutate(
      win_csum     = cumsum(win),
      loss_csum    = cumsum(loss),
      tie_csum     = cumsum(tie),
      games_played = 1:n(),
      win_pct      = (win_csum + tie_csum)/games_played
    ) %>% 
    dplyr::select(season, team, game_id, season_type, week, home_away, win, loss, tie, win_pct, games_played)
  
  return(team_results)
}

# Return clean rosters from nflfastR::fast_scraper() output
clean_rosters <- function(fsr) {
  
  logger::log_info("Cleaning {fsr$season[1]} rosters...")
  
  roster_df <- 
    fsr %>% 
    dplyr::select(player_id = gsis_id, full_name, season, 
                  team, position, birth_date, height, weight, years_exp) %>%
    dplyr::mutate(
      name        = gsub(" ", "_", tolower(
        gsub("[^[:alnum:]]+", " ",full_name)
      )),
      age         =  round(zoo::as.yearmon(Sys.Date()) - zoo::as.yearmon(birth_date), 2)
    ) %>% 
    dplyr::select(player_id, name, season, team, 
                  position, age, height, weight, years_exp) 
  
  return(roster_df)
}

# Calculate QB EPA per play and total EPA per game
get_qb_stats <- function(epa_pbp) {
  
  logger::log_info("Summarizing {epa_pbp$season[1]} QB stats...")
  
  qb_epa <- 
    epa_pbp %>% 
    dplyr::select(season, play_id, game_id, game_date, passer_player_name, passer_player_id, 
                  qb_epa, total_home_pass_epa,total_away_pass_epa) %>% 
    na.omit() %>% 
    dplyr::group_by(game_id, passer_player_name) %>% 
    dplyr::mutate(
      play_num = 1:n()
    ) %>% 
    dplyr::select(season, play_id, game_id, game_date, passer_player_name, passer_player_id, qb_epa) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(game_id, passer_player_name) %>% 
    dplyr::arrange(play_id, game_date, .by_group = T) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(game_id, passer_player_name) %>% 
    dplyr::mutate(
      total_epa       = cumsum(qb_epa),
      max_epa         = max(total_epa, na.rm =T),
      total_plays     = n()
    )  %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(season, game_id, game_date, passer_player_name) %>% 
    dplyr::summarize(
      qb_epa          = mean(max_epa, na.rm = T), 
      qb_epa_per_play = qb_epa/mean(total_plays, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(passer_player_name) %>% 
    dplyr::arrange(game_date, .by_group = T) %>% 
    dplyr::mutate(
      total_qb_epa       = cumsum(qb_epa)
    )
  return(qb_epa)
}
