nfl_teams <- function() {
  team_df <- tibble::tibble(
    team_name = c("Arizona Cardinals", "Atlanta Falcons" , "Baltimore Ravens",  "Buffalo Bills", 
                  "Carolina Panthers", "Chicago Bears",  "Cincinnati Bengals" ,"Cleveland Browns",
                  "Dallas Cowboys",  "Denver Broncos",  "Detroit Lions",  "Green Bay Packers", 
                  "Houston Texans","Indianapolis Colts",  "Jacksonville Jaguars", "Kansas City Chiefs",   
                  "Las Vegas Raiders",  "Oakland Raiders",  "Los Angeles Chargers", "San Diego Chargers", "Los Angeles Rams", "St. Louis Rams", 
                  "Miami Dolphins", 
                  "Minnesota Vikings",   "New England Patriots",  "New Orleans Saints", "New York Giants",         
                  "New York Jets",       "Philadelphia Eagles",  "Pittsburgh Steelers",  "San Francisco 49ers", 
                  "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team", "Washington Redskins"),
    team_abb  = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
                  "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LV", "LAC", "LAC",  "LA", "LA",  "MIA", "MIN", "NE", "NO", 
                  "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS","WAS")
  )
  return(team_df)
}

scrape_pfr <- function(year) {
  
  logger::log_info("\n\nScraping Pro Football Reference...\n{year} Rushing Defense ")
  year <- 2020
  url  <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fyears%2F", year, "%2Fopp.htm&div=div_rushing")
  
  page <- rvest::read_html(url)
  
  page_nodes <- 
    page %>%  
    rvest::html_nodes("table")
  
  page_table <- rvest::html_table(
    page_nodes[[1]],
    header  = T,
    fill    = T, 
    convert = T
  ) %>% 
    setNames(c("rank", "team", "games", "attempts", "rush_yards", 
               "rush_tds", "rush_yards_per_attempt", "rush_yards_per_game",
               "epa_rush_def")) %>% 
    dplyr::mutate(season = year) %>% 
    dplyr::filter(!team %in% c("League Total", "Avg Tm/G", "Avg Team")) %>% 
    dplyr::left_join(
      nfl_teams(),
      by = c("team" = "team_name")
    ) %>% 
    dplyr::select(team_abb, season, rank, rush_yards_per_attempt, rush_yards_per_game)
  
  return(page_table)
  
}

cumulative_mean <- function(x) {
  cummean <- (cumsum(x) / seq_along(x))
  return(cummean)
}

# is.nan() method for dataframes
is_nan_data_frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

# fill NA with column mean
mean_na <- function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}
# Function takes in Play by play dataset from nflfastR::load_pbp() and tidys --> adds win percentage
get_score_diff <- function(season_pbp) {
  
  season_year <-  season_pbp$season[1]
  
  logger::log_info("Calculating {season_year} win loss records...")
  
  homeaway <- 
    # season_pbp %>% 
    pbp %>% 
    dplyr::select(game_id, home_team, away_team) %>% 
    dplyr::group_by(game_id) %>% 
    dplyr::slice(1) %>%
    tidyr::pivot_longer(
      cols      = c(home_team, away_team),
      names_to  = "home_away",
      values_to = "team"
    ) %>% 
    dplyr::ungroup()
  
  # Slice off top row for each game to extract winners/loser
  season_pbp <-
    # season_pbp %>% 
    pbp %>%    
    # dplyr::select(season, game_id, play_id,
    #               home_team, away_team, season_type, week, home_score, away_score, drive, qtr,play_type, game_seconds_remaining,drive_time_of_possession,passing_yards,
    #               posteam, total_home_score,total_away_score, 
    #               result, score_differential, score_differential_post, roof, surface) %>%
    # dplyr::select(season, game_id, home_team, away_team, season_type, week, home_score, away_score,
    #               posteam, total_home_score,total_away_score, 
    #               result, score_differential, score_differential_post, roof, surface) %>%
    dplyr::group_by(game_id, posteam) %>%
    tidyr::pivot_longer(
      cols      = c(away_team, home_team),
      names_to  = "home_away",
      values_to = "team"
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::relocate(season, game_id, qtr,drive, play_id, play_type, game_seconds_remaining, drive_time_of_possession) %>%
    dplyr::group_by(game_id, qtr, posteam) %>% 
    dplyr::arrange(-game_seconds_remaining, .by_group = T) %>% 
    dplyr::mutate(
      drive_time_of_possession_sec =  
        60*as.numeric(sub(':.*', '', drive_time_of_possession)) + as.numeric(sub('.*:', '', drive_time_of_possession))
    ) %>% 
    dplyr::relocate(drive_time_of_possession_sec,drive_time_of_possession ) %>% 
    dplyr::ungroup()
  
  time_poss <- 
    season_pbp %>% 
    dplyr::filter(game_id == "2020_01_ARI_SF") %>%
    dplyr::group_by(game_id, qtr, posteam, drive) %>% 
    dplyr::summarize(
      mean_score_diff = mean(score_differential, na.rm = T),
      max_score_diff  = max(score_differential, na.rm = T),
      min_score_diff  = min(score_differential, na.rm = T),
      passing_yards   = sum(passing_yards, na.rm = T),
      time_of_poss    = mean(drive_time_of_possession_sec, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(game_id, qtr, posteam) %>% 
    dplyr::summarize(
      mean_score_diff = mean(mean_score_diff, na.rm = T),
      max_score_diff  = max(max_score_diff),
      min_score_diff  = min(min_score_diff),
      passing_yards   = sum(passing_yards, na.rm = T),
      time_of_poss    = sum(time_of_poss, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(posteam)) %>% 
    dplyr::group_by(game_id, qtr) %>% 
    dplyr::mutate(
      total_top = time_of_poss/sum(time_of_poss, na.rm = T)
    ) %>% 
    dplyr::mutate(across(where(is.numeric), round, 3))
    tidyr::pivot_wider(
      names_from = qtr,
      values_from = c(mean_score_diff, max_score_diff, min_score_diff, time_of_poss, passing_yards)
    )
  
  
}
# Function takes in Play by play dataset from nflfastR::load_pbp() and tidys --> adds win percentage
get_win_pct <- function(season_pbp) {

  season_year <-  season_pbp$season[1]
  
  logger::log_info("Calculating {season_year} win loss records...")

  homeaway <- 
    # season_pbp %>% 
    pbp %>% 
    dplyr::select(game_id, home_team, away_team) %>% 
    dplyr::group_by(game_id) %>% 
    dplyr::slice(1) %>%
    tidyr::pivot_longer(
      cols      = c(home_team, away_team),
      names_to  = "home_away",
      values_to = "team"
      ) %>% 
    dplyr::ungroup()

  # Slice off top row for each game to extract winners/loser
  season_pbp <-
    # season_pbp %>% 
    dplyr::select(season, game_id, play_id,
                  home_team, away_team, season_type, week, home_score, away_score, qtr,play_type, game_seconds_remaining,drive_time_of_possession,
                  posteam, total_home_score,total_away_score, 
                              result, score_differential, score_differential_post, roof, surface) %>%
    # dplyr::select(season, game_id, home_team, away_team, season_type, week, home_score, away_score,
    #               posteam, total_home_score,total_away_score, 
    #               result, score_differential, score_differential_post, roof, surface) %>%
    dplyr::group_by(game_id, posteam) %>% 
    tidyr::pivot_longer(
      cols      = c(away_team, home_team),
      names_to  = "home_away",
      values_to = "team"
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::relocate(season, game_id, team, home_away, qtr,play_id, play_type, game_seconds_remaining, drive_time_of_possession) %>% 
    dplyr::filter(game_id == "2020_01_ARI_SF") %>% 
    dplyr::group_by(game_id, qtr, posteam) %>% 
    dplyr::arrange(-game_seconds_remaining, .by_group = T) %>% 
    dplyr::mutate(
      drive_time_of_possession_sec =  
        60*as.numeric(sub(':.*', '', drive_time_of_possession)) + as.numeric(sub('.*:', '', drive_time_of_possession))
    ) %>% 
    dplyr::relocate(drive_time_of_possession_sec,drive_time_of_possession ) %>% 
    dplyr::summarize(
      max_score_diff = max(score_differential),
      min_score_diff = min(score_differential),
      time_of_poss   = sum(drive_time_of_possession_sec, na.rm = T)
    ) %>% 
    tidyr::pivot_wider(
      names_from = qtr,
      values_from = c(max_score_diff, min_score_diff, time_of_poss)
    )
  
    dplyr::group_by(game_id) %>%
    # dplyr::mutate(
    #   max_score_diff = max(score_differential, na.rm = T),
    #   min_score_diff = min(score_differential, na.rm = T)
    # ) %>% 
    # dplyr::filter(game_id == "1999_01_CIN_TEN") %>% 
    dplyr::filter(!is.na(score_differential)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(game_id, posteam) %>% 
    dplyr::mutate(
      max_score_diff = max(score_differential),
      min_score_diff = min(score_differential)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(season, game_id, 
                  home_away, team, posteam,
                  season_type, week, home_score, away_score,
                  result, max_score_diff, min_score_diff, roof, surface
                  # weather,  temp, wind
                  ) %>%
    # dplyr::group_by(game_id) %>%
    # dplyr::filter(!is.na(max_score_diff)) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(game_id, posteam) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-team, -home_away) %>% 
    dplyr::rename("team" = "posteam") %>% 
    dplyr::left_join(
      homeaway, 
      by = c("game_id", "team")
      )
  
  # Create Win/loss/tie columns
  team_results <- 
    season_pbp %>% 
    # tidyr::pivot_longer(cols      = c(away_team, home_team),  names_to  = "home_away",  values_to = "team" ) %>% 
    dplyr::group_by(game_id) %>% 
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
    dplyr::select(season, team, game_id, season_type, week, home_away, max_score_diff, min_score_diff,
                  roof, surface,
                  # weather,temp, wind, 
                  win, loss, tie, win_pct, games_played) %>% 
    dplyr::mutate(
      split_game_id = substr(game_id, 9, 20)
      ) %>% 
    dplyr::ungroup()
  
  # Replace changed team names from game ID
  team_results$split_game_id <- gsub("OAK", "LV", team_results$split_game_id)
  team_results$split_game_id <- gsub("SD", "LAC", team_results$split_game_id)
  team_results$split_game_id <- gsub("STL", "LA", team_results$split_game_id)

  team_results <- 
    team_results %>% 
    dplyr::mutate(
      home_team     =  str_split_fixed(split_game_id, "_", 2)[,2],
      away_team     =  str_split_fixed(split_game_id, "_", 2)[,1]
    ) %>% 
    dplyr::mutate(
      opponent  = case_when(
        home_team == team ~ away_team,
        away_team == team ~ home_team
      )
    ) %>%
    dplyr::select(-split_game_id, -home_team, -away_team) %>%
    dplyr::relocate(season, team, opponent) %>%
    dplyr::filter(team != "")
  
  return(team_results)
}

# Return clean rosters from nflfastR::fast_scraper() output
clean_rosters <- function(fscrape) {
  
  logger::log_info("Cleaning {fscrape$season[1]} rosters...")
  
  roster_df <- 
    fscrape %>%
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

# Add rolling mean to win % data
lag_win_pct <- function(
  df,
  periods = 8
                        ) {
  
  logger::log_info("Creating {periods} rolling mean on win %")
  
  lag_records <-
    df %>%
    # dplyr::select(season, week, team, win_pct) %>%
    dplyr::group_by(team) %>% 
    dplyr::arrange(season, week, .by_group = T) %>% 
    timetk::tk_augment_lags(win_pct, .lags = 1) %>%
    timetk::tk_augment_slidify(
      c(win_pct_lag1),
      .f = ~mean(.x, na.rm = T),
      .align = c("right"),
      .period  = c(periods),
      .partial = TRUE
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::select(team:games_played, win_pct_rmean = win_pct_lag1_roll_8) %>%
    dplyr::mutate(
      win_pct_rmean = case_when(
        is.nan(win_pct_rmean) & win_pct == 1 ~ 1,
        is.nan(win_pct_rmean) & win_pct == 0 ~ 0,
        TRUE                                 ~ win_pct_rmean
      )
    )
  
  return(lag_records)
}

# Add rolling mean to player stats
lag_player_stats <- function(
  df,
  periods = c(1, 2, 4, 8, 16)
) {
  
  logger::log_info("\n\nCreating {periods} week rolling mean on player stats")
  
  lag_players <-
    df %>%
    # dplyr::filter(carries > 0) %>%
    dplyr::group_by(player_id) %>% 
    dplyr::arrange(season, week, .by_group = T) %>% 
    timetk::tk_augment_lags(c(fp_rank, qb_touches:receiving_ypt), .lags = 1) %>% 
    timetk::tk_augment_slidify(
      contains("_lag1"),
      .f       = ~ mean(.x, na.rm = T),
      .align   = c("right"),
      .period  = c(periods),
      .partial = TRUE
    ) %>% 
    dplyr::ungroup()
  
  return(lag_players)
  # tmp_lag <-
  #   lag_players %>%
  #   dplyr::select(player_id:fp_hppr, contains("roll_"))
  # ggp <- 
  #   ggplot() +
  #   geom_line(data = tmp_rb, aes(x = n_game, y = rushing_yards), size = 2) + 
  #   geom_line(data = tmp_rb, aes(x = n_game, y = rushing_yards_lag1_roll_4), size = 1, color = "red") 
  # plotly::ggplotly(ggp)

}

# Add rolling mean to player stats
lag_team_stats <- function(
  df,
  periods = c(1, 2, 4, 8, 16)
) {
  
  logger::log_info("\n\nCreating {periods} week rolling mean on player stats")
  
  lag_team <-
    df %>%
    # team_positions
    dplyr::group_by(team, position) %>% 
    dplyr::arrange(season, week, .by_group = T) %>% 
    timetk::tk_augment_lags(c(fp_rank, qb_touches:receiving_ypt), .lags = 1) %>% 
    timetk::tk_augment_slidify(
      contains("_lag1"),
      .f       = ~ mean(.x, na.rm = T),
      .align   = c("right"),
      .period  = c(periods),
      .partial = TRUE
    ) %>% 
    dplyr::ungroup()
  
  return(lag_team)
  # tmp_lag <-
  #   lag_players %>%
  #   dplyr::select(player_id:fp_hppr, contains("roll_"))
  # ggp <- 
  #   ggplot() +
  #   geom_line(data = tmp_rb, aes(x = n_game, y = rushing_yards), size = 2) + 
  #   geom_line(data = tmp_rb, aes(x = n_game, y = rushing_yards_lag1_roll_4), size = 1, color = "red") 
  # plotly::ggplotly(ggp)
  
}
# Calculate QB EPA per play and total EPA per game
get_qb_stats <- function(epa_pbp) {

  logger::log_info("\n\nSummarizing {epa_pbp$season[1]} QB stats...")
 # epa_pbp <- pbp
 # 
  qb_epa <- 
    epa_pbp %>%
    # pbp %>% 
    dplyr::select(season, week, play_id, game_id, game_date, passer_player_name, passer_player_id,
                  player_id = id,
                  # pass_length, passing_yards, series_result,
                  air_yards, complete_pass,
                  incomplete_pass, success, 
                  qb_epa, total_home_pass_epa,total_away_pass_epa) %>% 
    dplyr::mutate(
      pass_length = case_when(
        air_yards >= 15 ~ "deep",
        air_yards < 15  ~ "short"
        )
      ) %>% 
    na.omit() 
  
  if(nrow(qb_epa > 0)) {
    
    qb_epa <-
      qb_epa %>% 
      dplyr::group_by(game_id, passer_player_name) %>% 
      dplyr::select(season, week, play_id, game_id, game_date, passer_player_name, passer_player_id,  player_id,
                    pass_length, air_yards, complete_pass, incomplete_pass, success, qb_epa) %>% 
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
      dplyr::group_by(season, game_id, game_date, passer_player_name, pass_length) %>% 
      dplyr::mutate(
        pass_attempt    = n(),
        pass_complete   = sum(complete_pass, na.rm = T)
        ) %>% 
      tidyr::pivot_wider(
        names_from      = "pass_length", 
        names_glue      = "{pass_length}_{.value}",
        values_from     = c(pass_attempt, pass_complete)
        ) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(season, week, game_id, game_date, passer_player_name, player_id) %>%
      dplyr::summarize(
        success              = sum(success),
        air_yards            = sum(air_yards, na.rm = T), 
        deep_pass_attempt    = mean(deep_pass_attempt, na.rm = T),
        deep_pass_complete   = mean(deep_pass_complete, na.rm = T),
        short_pass_attempt   = mean(short_pass_attempt, na.rm = T),
        short_pass_complete  = mean(short_pass_complete, na.rm = T),
        qb_epa               = mean(max_epa, na.rm = T), 
        qb_epa_per_play      = qb_epa/mean(total_plays, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      tidyr::replace_na(
        list(
          deep_pass_attempt   = 0, 
          deep_pass_complete  = 0,
          short_pass_attempt  = 0, 
          short_pass_complete = 0
          )
        ) %>% 
      dplyr::group_by(passer_player_name) %>% 
      dplyr::arrange(game_date, .by_group = T) %>% 
      dplyr::mutate(
        total_qb_epa       = cumsum(qb_epa)
      ) %>%  
      dplyr::ungroup()
  
    return(qb_epa) 
    
  } else {
    
    logger::log_info("\n\nSkipping {epa_pbp$season[1]} due to missing data\n---> returning NULL value")
    
    # return(NULL)
    
  }
}
