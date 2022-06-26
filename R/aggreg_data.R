# Angus Watters 
# Aggregate and clean data from nflFastR

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(timetk)

source("utils/utils.R")

data_path  <-  here::here("data")

# ********************
# ---- NFL Fast R ----
# ********************

# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR

# Question 1: what QB stats most influence W/L
# Question 2: Factors that influence whether a RB will finish in top 5 on a week?
 # Team Win %
 # Height
 # Weight
 # Snap % 
 # % RB carries
 # Yards per route run 
 # Yard per carry
 # Number of top 5 finishes so far

# **********************
# ---- Player stats ----
# **********************

# Data dictionary
desc <- nflfastR::field_descriptions

# Load player stats
stats       <- nflfastR::load_player_stats()
dplyr::glimpse(stats)

# unique seasons
seasons_lst <- unique(stats$season)

# **********************
# ---- QB EPA stats ----
# **********************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
qb_stats <- lapply(seasons_lst, FUN = function(x) {
  logger::log_info("Retrieving {x} weekly player stats...")
  pbp <- nflfastR::load_pbp(x) %>% 
    # nflfastR::add_qb_epa() %>% 
    calculate_player_stats(weekly = TRUE) %>% 
    dplyr::rename("team" = "recent_team")
    # get_qb_stats()
  }
  ) %>%
  dplyr::bind_rows()

saveRDS(qb_stats, here::here("data", "weekly_player.rds"))
# **********************
# ---- Team records ----
# **********************

# pull rosters for every year
team_records <- lapply(seasons_lst, FUN = function(x) {
  pbp <- nflfastR::load_pbp(2020) %>% 
    get_win_pct()
  }
  ) %>%
  dplyr::bind_rows()

saveRDS(team_records, here::here("data", "team_records.rds"))

# ***********************
# ---- Clean rosters ----
# ***********************

# pull rosters for every year
rosters <- lapply(seasons_lst, FUN = function(x) {
 fsr <- fast_scraper_roster(x)  %>% 
    clean_rosters()
  }
  ) %>% 
  dplyr::bind_rows() 


saveRDS(rosters, here::here("data", "rosters.rds"))
rosters <- readRDS(here::here("data", "rosters.rds"))
# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************

qb_stats <- readRDS(here::here("data", "weekly_player.rds"))

rosters  <- readRDS(here::here("data", "rosters.rds"))

# subset roster columns 
rost <- 
  rosters %>% 
  dplyr::ungroup() %>%  
  dplyr::select(season, team, player_id, position)

# subset win loss columns 
wl <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, team, opponent, win)

# Win loss. passing stats, 
wl_stats <- 
  qb_stats %>% 
  dplyr::left_join(
    dplyr::select(rost, season, player_id, position), 
    by = c("season", "player_id")
  ) %>% 
  dplyr::left_join(
    wl,
    by = c("team", "season", "week")
  ) %>% 
  dplyr::relocate(season, week, team,opponent, win, player_id, player_name, position)

saveRDS(wl_stats, here::here("data", "weekly_player_team_record.rds"))

# ****************************************
# ---- Weekly Cumalative player stats ----
# ****************************************

# Team records/schedule
team_records <-  readRDS(here::here("data", "team_records.rds"))

lag_records <- 
  team_records %>%
  lag_win_pct(periods = 8)

# Weekly player stats
player_stats <- readRDS(here::here("data", "weekly_player_team_record.rds"))

# Weekly Top finishers
week_finish <- 
  player_stats %>% 
  dplyr::filter(position %in% c("QB", "WR", "TE", "RB")) %>% 
  dplyr::group_by(season, week, position) %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5),
    fp_rank   = rank(-fp_hppr,   ties.method = "min"),
    fp_finish = case_when(
      fp_rank <= 5 ~ "top_5",
      TRUE         ~ "not_top_5"
    )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::relocate(season, week, team, opponent, win, fp_hppr, fp_rank, fp_finish) %>% 
  # dplyr::filter(season == 2021, week %in% c(1, 2, 3, 4)) %>% 
  dplyr::select(season, week, team, opponent, position, player_name, player_id, fp_hppr, fp_rank, 
                fp_finish, completions:receiving_2pt_conversions) %>% 
  dplyr::left_join(
    dplyr::select(lag_records, season, team, week, win_pct_rmean, home_away),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::relocate(season, week, team, opponent, win_pct_rmean, home_away, position, player_name, player_id, fp_hppr, fp_rank, fp_finish) 

# Cumalive stats
# cuml_player_stats <-
#   week_finish %>% 
#   # dplyr::filter(season == 2021 & week %in% c(1:10) | season == 2020 & week %in% c(1:16), position == "RB") %>%
#   dplyr::select(season, week, team, opponent, win_pct, position, player_name, player_id,
#                 fp_hppr, fp_rank, fp_finish,
#                 completions:receiving_2pt_conversions) %>%
#   dplyr::mutate(across(where(is.numeric), round, 3)) %>%
#   dplyr::group_by(season, player_id) %>%
#   dplyr::arrange(week, .by_group = T) %>%
#   dplyr::mutate(across(c(completions:receiving_2pt_conversions), ~ lag(cumulative_mean(.x)), .names = "{col}")) %>%
#   dplyr::mutate(
#     comp_pct            = completions/attempts,
#     pass_yards_pa       = passing_yards/attempts,
#     rush_yards_pc       = rushing_yards/carries,
#     receiving_yards_pt  = receiving_yards/targets,
#     receiving_yards_pr  = receiving_yards/receptions,
#   ) %>%
#   dplyr::ungroup()

# Cumalive stats
cuml_player_stats <-
  week_finish %>% 
  dplyr::group_by(season, player_id, position) %>% 
  dplyr::mutate(
    fp_rank_season = mean(fp_rank, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, week, position) %>%
  dplyr::filter(fp_rank_season <= 36) %>% 
  dplyr::ungroup() %>% 
  # dplyr::filter(season == 2021 & week %in% c(1:10) | season == 2020 & week %in% c(1:16)) %>%
  # dplyr::select(season, week, team, opponent, win_pct_rmean, position, player_name, player_id,
  #               fp_hppr, 
  #               # fp_rank, fp_finish, 
  #               carries, rushing_yards, targets, receptions, receiving_yards
  #               ) %>% 
  # dplyr::select(season, week, team, opponent, win_pct, position, player_name, player_id,
  #               fp_hppr, fp_rank, fp_finish,
  #               completions:receiving_2pt_conversions) %>% 
  dplyr::group_by(player_id) %>% 
  dplyr::arrange(season, week, .by_group = T) %>% 
  dplyr::mutate(
    touches             = carries + receptions,
    qb_touches          = attempts + carries + receptions
    ) %>% 
  # dplyr::filter(qb_touches > 1) %>%
  dplyr::mutate(
    comp_pct            = completions/attempts,
    pass_ypa            = passing_yards/attempts,
    pass_ypc            = passing_yards/completions,
    rush_ypc            = rushing_yards/carries,
    receive_ypr         = receiving_yards/receptions,
    receiving_ypt       = receiving_yards/targets,
    total_yards         = rushing_yards + receiving_yards,
    ypt                 = total_yards/touches,
    qb_total_yards      = passing_yards + rushing_yards + receiving_yards,
    qb_ypt              = qb_total_yards/qb_touches,
    qb_adot             = passing_air_yards/attempts,
    receive_adot        = receiving_air_yards/targets,
    catch_rate          = receptions/targets
  ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::ungroup() %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(-fp_rank_season) %>% 
  dplyr::relocate(season:fp_finish,  qb_touches, qb_total_yards, qb_ypt, completions, attempts, comp_pct, qb_adot, pass_ypa, pass_ypc,
                  passing_yards:dakota, 
                  touches, total_yards, ypt,  carries, rushing_yards, rush_ypc, rushing_tds:rushing_2pt_conversions,
                  receptions, targets, catch_rate, receive_adot,  receiving_yards:receiving_ypt
                  )

player_stats_rmean <- 
  cuml_player_stats %>% 
  lag_player_stats(periods = c(1, 2, 4, 8))
# tmp_wr <- cuml_player_stats %>% 
#   dplyr::filter(position == "WR") %>% 
#   dplyr::group_by(player_name, player_id) %>% 
#   dplyr::summarise( total_targets  = sum(targets, na.rm = T),  receive_adot = mean(receive_adot, na.rm = T) ) %>%
#   dplyr::filter(total_targets > 600)
# hist(tmp_wr$total_targets, breaks = 50)

# convert NaN to 0
# player_stats_rmean[is_nan_data_frame(player_stats_rmean)] <- 0

# tmp <- fp_df[rowSums(is.na(fp_df)) > 0, ]
# 
# length(unique(cuml_player_stats$touches))
# cnt_touch <- count(cuml_player_stats, qb_touches)

hist(cuml_player_stats$touches, breaks = 20)
plot(cuml_player_stats$fp_hppr~cuml_player_stats$rushing_tds)
# readRDS(here::here("data", "player_stats_rollmean.rds"))
# # Save
# saveRDS(cuml_player_stats, here::here("data", "cumulative_player_stats.rds"))
# Save
saveRDS(player_stats_rmean, here::here("data", "player_stats_rollmean.rds"))

# *******************
# ---- Team wins ----
# *******************
team_records <-  readRDS(here::here("data", "team_records.rds"))

# season total wins
win_total  <- 
  team_records %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    win  = sum(win, na.rm = T)
  )

# Weekly player stats
player_stats <- readRDS(here::here("data", "weekly_player_team_record.rds"))

# Weekly team wins
team_stats <- 
  player_stats %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, week, team) %>% 
  # dplyr::mutate( fumbles = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost )
  dplyr::summarise(
    across(c(completions:fp_hppr), sum, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(    
    fumbles   = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost,
    turnovers = fumbles + interceptions
  ) %>% 
  dplyr::left_join(
    dplyr::select(team_records, season, team, opponent, home_away, week,max_score_diff, min_score_diff,roof, surface, win),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::select(
    -rushing_fumbles_lost, -sack_fumbles_lost, -receiving_fumbles_lost,
    ) %>% 
  dplyr::mutate(
    roof = case_when(
      roof %in% c("dome", "closed") ~ 1,
      TRUE                          ~ 0,
    ),
    grass = case_when(
      surface == "grass" ~ 1,
      TRUE               ~ 0
    )
  ) %>% 
  dplyr::select(-surface) %>% 
  dplyr::relocate(season, week, team, opponent, home_away)

saveRDS(team_stats, here::here("data", "nfl_wins.rds"))

# *****************************
# ---- Weekly Team Defense ----
# *****************************

team_stats <- readRDS(here::here("data", "nfl_wins.rds"))

team_defense <- 
  team_stats %>% 
  dplyr::select(season, week, team, opponent, 
                attempts, completions, carries, receptions, targets, passing_yards, 
                rushing_yards, receiving_yards, passing_tds, rushing_tds, passing_epa,  rushing_epa, receiving_epa) %>%
  dplyr::group_by(opponent) %>%
  dplyr::mutate(
    touches             = carries + receptions, 
    comp_pct            = completions/attempts,
    catch_rate          = receptions/targets,
    pass_ypa            = passing_yards/attempts,
    pass_ypc            = passing_yards/completions,
    rush_ypc            = rushing_yards/carries,
    receive_ypr         = receiving_yards/receptions,
    receiving_ypt       = receiving_yards/targets,
    total_yards         = rushing_yards + receiving_yards,
    ypt                 = total_yards/touches,
    qb_total_yards      = passing_yards + rushing_yards + receiving_yards
 
  ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::ungroup() %>% 
  replace(is.na(.), 0) %>% 
  # dplyr::mutate(
  #   comp_pct   = opp_completions/opp_attempts,
  #   rush_ypc   = opp_rushing_yards/opp_carries,
  #   pass_ypa   = opp_passing_yards/opp_attempts
  # ) %>% 
  dplyr::group_by(opponent) %>%
  # dplyr::arrange(season, week, .by_group = T) %>% 
  # timetk::tk_augment_lags(attempts:qb_total_yards, .lags = 1) %>% 
  # timetk::tk_augment_slidify(
  #   contains("_lag1"),
  #   .f       = ~ mean(.x, na.rm = T),
  #   .align   = c("right"),
  #   .period  = c(1, 2, 4, 8),
  #   .partial = TRUE
  # ) %>% 
  # dplyr::ungroup() %>% 
  # dplyr::select(season, week, team, opponent, contains("roll"))

# rm(team_defense)
# team_defense <-
#   team_defense %>% 
#   setNames(c("season", "week", "team", "opponent", paste0("opp_", names(team_defense)[5:100]))) %>% 
#   dplyr::select(-team) %>% 
#   dplyr::rename(team = opponent)
  
# dplyr::mutate(
#     opp_comp_pct        = opp_completions/opp_attempts,
#     opp_rush_yards_pc   = opp_rushing_yards/opp_carries,
#     opp_pass_yards_pa   = opp_passing_yards/opp_attempts
#   ) %>% 
  # dplyr::mutate(across(where(is.numeric), round, 2)) %>%
  # dplyr::ungroup() %>%
  # dplyr::select(season, week, opponent, opp_attempts:opp_pass_yards_pa) %>%
  # dplyr::rename(team = opponent)

  dplyr::arrange(season, week, .by_group = T) %>%
  dplyr::mutate(across(c(attempts:rushing_tds), ~ lag(cumulative_mean(.x)), .names = "opp_{col}")) %>%
  dplyr::mutate(across(c(where(is.numeric)), ~ mean_na(.x))) %>% 
  dplyr::mutate(
    opp_comp_pct        = opp_completions/opp_attempts,
    opp_rush_yards_pc   = opp_rushing_yards/opp_carries,
    opp_pass_yards_pa   = opp_passing_yards/opp_attempts
  ) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, week, opponent, opp_attempts:opp_pass_yards_pa) %>%
  dplyr::rename(team = opponent)

# Save
saveRDS(team_defense, here::here("data", "team_defense_cuml.rds"))
# saveRDS(team_defense, here::here("data", "team_defense_rollmean.rds"))

# ***************************
# ---- Weekly FP allowed ----
# ***************************

player_stats <- readRDS(here::here("data", "weekly_player_team_record.rds"))

# Weekly Schedule
sched <- 
  player_stats %>% 
  # dplyr::filter(position %in% c("QB", "WR", "TE", "RB")) %>%
  dplyr::select(season, week, team, opponent) %>% 
  dplyr::group_by(season, week, team, opponent) %>% 
  dplyr::summarize() %>% 
  dplyr::ungroup() 

fp_against <- 
  player_stats %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5)
  ) %>% 
  dplyr::select(season:position, fp_hppr) %>% 
  dplyr::filter(position %in% c("QB", "WR", "TE", "RB")) %>%
  dplyr::group_by(season, week, team, position) %>% 
  dplyr::summarise(
  # dplyr::mutate(
    fp_hppr = sum(fp_hppr,na.rm = T)
    ) %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(
    sched,
    by = c("season","week", "team")
  ) %>% 
  dplyr::relocate(season, week, team, opponent, position, fp_hppr) %>% 
  # dplyr::group_by(season, opponent, position) %>%
  # dplyr::arrange(season, week, .by_group = T) %>%
  dplyr::group_by(opponent, position) %>%
  dplyr::arrange(season, week, .by_group = T) %>%
  # timetk::tk_augment_lags(fp_hppr, .lags = 1) %>% 
  # timetk::tk_augment_slidify(
  #   contains("_lag1"),
  #   .f       = ~ mean(.x, na.rm = T),
  #   .align   = c("right"),
  #   .period  = c(1, 2, 4, 8),
  #   .partial = TRUE
  # ) %>% 
  # dplyr::ungroup()%>% 
  # dplyr::mutate(across(c(fp_hppr), ~ cumsum(.x), .names = "opp_{col}")) 
  dplyr::mutate(across(c(fp_hppr), ~ lag(cumulative_mean(.x)), .names = "opp_{col}")) %>%
  tidyr::pivot_wider(
    id_cols     = c(season, week, opponent), 
    names_from  = "position",
    # values_from = c(fp_hppr_lag1_roll_1, fp_hppr_lag1_roll_2, fp_hppr_lag1_roll_4, fp_hppr_lag1_roll_8)
    names_glue  = "{position}_{.value}",
    values_from = "opp_fp_hppr"
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(opponent, season) %>% 
  dplyr::mutate(across(c(where(is.numeric)), ~ mean_na(.x))) %>% 
  dplyr::ungroup() %>% 
  janitor::clean_names() %>% 
  dplyr::rename(team = opponent)
# %>% 
#   dplyr::select(season, week, team = opponent,
#                 opp_qb_fp = QB, opp_rb_fp = RB,
#                 opp_wr_fp = WR, opp_te_fp = TE)

# saveRDS(fp_against, here::here("data", "team_fp_against.rds"))
# saveRDS(fp_against, here::here("data", "team_fp_against_rollmean.rds"))
saveRDS(fp_against, here::here("data", "team_fp_against_cuml.rds"))

# ********************************
# ---- Final player data join ----
# ********************************
#  Final cumalative weekly player stats

# Team records/schedule
team_records      <-  readRDS(here::here("data", "team_records.rds"))

# Weekly Team defense
team_defense      <- readRDS(here::here("data", "team_defense_cuml.rds"))
# team_defense      <- readRDS(here::here("data", "team_defense.rds"))
# team_defense      <- readRDS(here::here("data", "team_defense_rollmean.rds"))

# Weekly Team Fantasy points allowed by position
# fp_against        <- readRDS(here::here("data", "team_fp_against.rds"))
fp_against        <- readRDS(here::here("data", "team_fp_against_cuml.rds"))
# fp_against        <- readRDS(here::here("data", "team_fp_against_rollmean.rds"))

# Weekly player stats
player_stats      <- readRDS(here::here("data", "weekly_player_team_record.rds"))
  # dplyr::filter(position == "RB")

# Cumlative Weekly player stats
# cuml_player_stats <-  readRDS(here::here("data", "cumulative_player_stats.rds"))
player_stats_rmean <-  readRDS(here::here("data", "player_stats_rollmean.rds"))

# Opponents defense stats & FP against
opponent_defense <- 
  team_defense %>% 
  dplyr::left_join(
    fp_against,
    by = c("team", "week", "season")
    ) %>% 
  dplyr::rename(opponent = team)

fp_model_data <- 
  # cuml_player_stats %>% 
  player_stats_rmean %>% 
  dplyr::left_join(
    opponent_defense,
    by = c("opponent", "week", "season")
    )


# saveRDS(fp_model_data, here::here("data", "fp_model_data.rds"))
saveRDS(fp_model_data, here::here("data", "fp_rollmean_data.rds"))

# *************************************
# ---- Aggregate positions by team ----
# *************************************
# Team records/schedule
team_records <-  readRDS(here::here("data", "team_records.rds"))

lag_records <- 
  team_records %>%
  lag_win_pct(periods = 8)

# Weekly player stats
player_stats <- readRDS(here::here("data", "weekly_player_team_record.rds"))

# Weekly Top finishers
team_finish <- 
  player_stats %>% 
  dplyr::filter(position %in% c("QB", "WR", "TE", "RB")) %>% 
  dplyr::group_by(season, week, position) %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5),
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::relocate(season, week, team, opponent, win, fp_hppr) %>% 
  # dplyr::filter(season == 2021, week %in% c(1, 2, 3, 4)) %>% 
  dplyr::select(season, week, team, opponent, position, player_name, player_id, fp_hppr, completions:receiving_2pt_conversions) %>% 
  dplyr::left_join(
    dplyr::select(lag_records, season, team, week, win_pct_rmean, home_away),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::relocate(season, week, team, opponent, win_pct_rmean, home_away, position, player_name, player_id, fp_hppr) 

team_positions <- 
  team_finish %>% 
  dplyr::group_by(season, week, team, position) %>% 
  dplyr::summarise(
    across(c(fp_hppr, completions:receiving_2pt_conversions), sum, na.rm = T)
    ) %>% 
  dplyr::left_join(
    dplyr::summarize(
      dplyr::group_by(
        dplyr::select(player_stats, season, week, team, opponent),
        season, week, team, opponent
      )
    ), 
    by = c("season", "week", "team")
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(
    dplyr::select(lag_records, season, team, week, win_pct_rmean, home_away),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::group_by(season, week, position) %>% 
  dplyr::mutate(
    fp_rank   = rank(-fp_hppr,   ties.method = "min")
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(team, position) %>% 
  dplyr::arrange(season, week, .by_group = T) %>% 
  dplyr::mutate(
    touches             = carries + receptions,
    qb_touches          = attempts + carries + receptions
  ) %>% 
  # dplyr::filter(qb_touches > 1) %>%
  dplyr::mutate(
    comp_pct            = completions/attempts,
    pass_ypa            = passing_yards/attempts,
    pass_ypc            = passing_yards/completions,
    rush_ypc            = rushing_yards/carries,
    receive_ypr         = receiving_yards/receptions,
    receiving_ypt       = receiving_yards/targets,
    total_yards         = rushing_yards + receiving_yards,
    ypt                 = total_yards/touches,
    qb_total_yards      = passing_yards + rushing_yards + receiving_yards,
    qb_ypt              = qb_total_yards/qb_touches,
    qb_adot             = passing_air_yards/attempts,
    receive_adot        = receiving_air_yards/targets,
    catch_rate          = receptions/targets
  ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  dplyr::ungroup() %>% 
  replace(is.na(.), 0) %>% 
  dplyr::relocate(season, week, team, opponent, home_away, position, fp_hppr, fp_rank, 
                  qb_touches, qb_total_yards, qb_ypt, completions, attempts, comp_pct, qb_adot, pass_ypa, pass_ypc,
                  passing_yards:dakota, 
                  touches, total_yards, ypt,  carries, rushing_yards, rush_ypc, rushing_tds:rushing_2pt_conversions,
                  receptions, targets, catch_rate, receive_adot,  receiving_yards:receiving_ypt
  )

summary(lm(fp_hppr~., data = team_positions))

saveRDS(team_positions, here::here("data", "team_stats.rds"))


team_positions_cuml <- 
  team_positions %>% 
  dplyr::group_by(position, team, season) %>% 
  dplyr::arrange(season, week, .by_group = T) %>% 
  dplyr::mutate(across(c(fp_rank:receiving_ypt), ~ lag(cumulative_mean(.x)), .names = "avg_{col}")) %>%
  dplyr::mutate(across(c(where(is.numeric)), ~ mean_na(.x))) %>% 
  dplyr::ungroup()



saveRDS(team_positions_cuml, here::here("data", "team_stats_cuml.rds"))

# tmp <- 
#   team_positions_cuml %>% 
#   dplyr::ungroup() %>% 
#   dplyr::group_by(season, team, position) %>% 
#   dplyr::summarise(total_fp = mean(fp_hppr, na.rm = T))

lag_team_positions <-
  team_positions %>% 
  lag_team_stats()

# Save
saveRDS(lag_team_positions, here::here("data", "team_stats_rollmean.rds"))

# ******************************
# ---- Final team data join ----
# ******************************
#  Final cumalative weekly player stats

# Team records/schedule
team_records      <-  readRDS(here::here("data", "team_records.rds"))

# Weekly Team defense
# Weekly Team defense
team_defense      <- readRDS(here::here("data", "team_defense_cuml.rds"))
# team_defense      <- readRDS(here::here("data", "team_defense.rds"))
# team_defense      <- readRDS(here::here("data", "team_defense_rollmean.rds"))

# Weekly Team Fantasy points allowed by position
# fp_against        <- readRDS(here::here("data", "team_fp_against.rds"))
fp_against        <- readRDS(here::here("data", "team_fp_against_cuml.rds"))
# Weekly player stats
# team_stats      <- readRDS(here::here("data", "team_stats.rds"))
team_stats      <- readRDS(here::here("data", "team_stats_cuml.rds"))
# player_stats      <- readRDS(here::here("data", "weekly_player_team_record.rds"))
# dplyr::filter(position == "RB")

# Cumlative Weekly player stats
# cuml_player_stats <-  readRDS(here::here("data", "cumulative_player_stats.rds"))
# lag_team_positions <-  readRDS(here::here("data", "team_stats_rollmean.rds"))

# Opponents defense stats & FP against
opponent_defense <- 
  team_defense %>% 
  dplyr::left_join(
    fp_against,
    by = c("team", "week", "season")
  ) %>% 
  dplyr::rename(opponent = team)

fp_team_model_data <- 
  # cuml_player_stats %>% 
  # lag_team_positions %>% 
  team_stats %>% 
  dplyr::left_join(
    opponent_defense,
    by = c("opponent", "week", "season")
  ) %>% 
  dplyr::mutate(
    home = case_when(
      home_away == "home_team"  ~ 1,
      home_away == "away_team"  ~ 0 
    )
  ) %>% 
  dplyr::mutate(
    # fp_finish = factor(fp_finish, levels = c(1, 0)),
    home      = factor(home, levels = c(1, 0))
  ) %>% 
  dplyr::select(-home_away) %>% 
  dplyr::relocate(team,position, season, week,opponent, home) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::ungroup()

saveRDS(fp_team_model_data, here::here("data", "fp_team_cuml_data.rds"))

# saveRDS(fp_model_data, here::here("data", "fp_model_data.rds"))
# saveRDS(fp_team_model_data, here::here("data", "fp_team_rollmean_data.rds"))
# x <- datasets::mtcars %>%
fp_corr <-
  fp_team_model_data %>% 
  dplyr::select(where(is.numeric)) %>% 
  dplyr::select(fp_hppr, contains("avg_")) %>% 
  corrr::correlate() %>%    # Create correlation data frame (cor_df)
  # corrr:::focus(-cyl, -vs, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
  corrr:::rearrange() %>%  # rearrange by correlations
  corrr::shave() %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) 

corr_plot <- 
  fp_corr %>% 
  corrr::rplot() +
  theme(
    axis.text.x = element_text(angle = 300)
    ) 

plotly::ggplotly(corr_plot)
fp_corr
# class(fp_corr)

  # *************************************************
# *************************************************

qb_epa <-
  qb_epa %>%
  rename("player_name" = passer_player_name)

qb_stats <- left_join(
  qb_epa,  
  # dplyr::select(stats, season, week, team = recent_team, player_name, player_id, 7:12),
  dplyr::select(stats, season, week, team = recent_team, player_id, 7:12),
  by = c("season", "week", "player_id")
  )
library(PerformanceAnalytics)
qb_stats2 <- 
  qb_stats %>% 
  rename("player_name" = passer_player_name) %>% 
  dplyr::select(season, week,team,  game_id, game_date, player_name, player_id,
                success, completions, attempts, passing_yards, air_yards, passing_tds, interceptions, sacks,
                deep_pass_complete, deep_pass_attempt,  short_pass_complete, short_pass_attempt, 
                qb_epa, qb_epa_per_play, total_qb_epa
                ) %>% 
  dplyr::mutate(
    season         = as.character(season), 
    week           = as.character(week), 
    comp_pct       = completions/attempts,
    deep_comp_pct  = deep_pass_complete/deep_pass_attempt,
    short_comp_pct = short_pass_complete/short_pass_attempt
  ) %>% 
  dplyr::group_by(season, week, game_id, team) %>% 
  dplyr::mutate(across(success:short_pass_attempt, sum)) %>% 
  dplyr::group_by(season, week, game_id, team) %>% 
  dplyr::summarise(across(where(is.numeric), mean))

# ****************************
# ---- QB by winloss ----
# ****************************
wl <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, team, win)

rost <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::select(team,game_id, win)
wl <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, team, win)

left_join(stats_roster,
          wl,
          by = c("team", "season", "week")
) 
# ****************************
# ---- Games Tidy Tuesday ----
# ****************************

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

games <- 
  games %>% 
  dplyr::mutate(
    gamename = gsub("[^[:alnum:]]+", "_",
                    stringr::str_trim(
                      gsub(".*\\((.*)\\).*", "\\1",
                           tolower(
                             gsub("[<].*[>]", "", gamename)
                           )
                      ))),
    avg_peak_perc = as.numeric(gsub("%", "", avg_peak_perc)),
    month_num     = as.integer(factor(month, levels = month.name)),
    date          = as.Date(paste0(year, "-", month_num, "-01"))
  ) %>% 
  dplyr::select(gamename, date, year, month, avg, gain, peak, avg_peak_perc)


    

