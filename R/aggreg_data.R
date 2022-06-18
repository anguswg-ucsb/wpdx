# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR & Tidy Tuesday 2012-05-04 Steam Games dataset

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(httr)
library(jsonlite)
library(rvest)
library(corrplot)
library(plotly)
library(viridis)
library(olsrr)

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
  pbp <- nflfastR::load_pbp(x) %>% 
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
    dplyr::select(team_records, season, team, week, win_pct),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::relocate(season, week, team, opponent, win_pct, position, player_name, player_id, fp_hppr, fp_rank, fp_finish) 

# Cumalive stats
cuml_player_stats <-
  week_finish %>% 
  # dplyr::filter(season == 2021, week %in% c(1, 2, 3, 4)) %>% 
  dplyr::select(season, week, team, opponent, win_pct, position, player_name, player_id,
                fp_hppr, fp_rank, fp_finish,
                completions:receiving_2pt_conversions) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::group_by(season, player_id) %>% 
  dplyr::arrange(week, .by_group = T) %>% 
  dplyr::mutate(across(c(completions:receiving_2pt_conversions), ~ lag(cumulative_mean(.x)), .names = "{col}")) %>% 
  dplyr::mutate(
    comp_pct        = completions/attempts,
    rush_yards_pc   = rushing_yards/carries,
    pass_yards_pa   = passing_yards/attempts
  ) %>% 
  dplyr::ungroup()

# Save
saveRDS(cuml_player_stats, here::here("data", "cumulative_player_stats.rds"))

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
                attempts, completions, carries, receptions,  passing_yards, 
                rushing_yards, passing_epa,  rushing_epa, receiving_epa) %>% 
  dplyr::group_by(season, opponent) %>%
  dplyr::arrange(week, .by_group = T) %>% 
  dplyr::mutate(across(c(attempts:rushing_yards), ~ lag(cumulative_mean(.x)), .names = "opp_{col}")) %>% 
  dplyr::mutate(
    opp_comp_pct        = opp_completions/opp_attempts,
    opp_rush_yards_pc   = opp_rushing_yards/opp_carries,
    opp_pass_yards_pa   = opp_passing_yards/opp_attempts
  ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, opponent, opp_attempts:opp_pass_yards_pa) %>% 
  dplyr::rename(team = opponent)

saveRDS(team_defense, here::here("data", "team_defense.rds"))

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
  dplyr::group_by(season, opponent, position) %>% 
  dplyr::arrange(week, .by_group = T) %>% 
  # dplyr::mutate(across(c(fp_hppr), ~ cumsum(.x), .names = "opp_{col}")) 
  dplyr::mutate(across(c(fp_hppr), ~ lag(cumulative_mean(.x)), .names = "opp_{col}")) %>% 
  tidyr::pivot_wider(
    id_cols     = c(season, week, opponent), 
    names_from  = "position",
    # names_glue  = "{position}_{.value}",
    values_from = "opp_fp_hppr"
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, team = opponent,
                opp_qb_fp = QB, opp_rb_fp = RB,
                opp_wr_fp = WR, opp_te_fp = TE)


saveRDS(fp_against, here::here("data", "team_fp_against.rds"))


# ********************************
# ---- Final player data join ----
# ********************************
#  Final cumalative weekly player stats

# Team records/schedule
team_records      <-  readRDS(here::here("data", "team_records.rds"))

# Weekly Team defense
team_defense      <- readRDS(here::here("data", "team_defense.rds"))

# Weekly Team Fantasy points allowed by position
fp_against        <- readRDS(here::here("data", "team_fp_against.rds"))

# Weekly player stats
player_stats      <- readRDS(here::here("data", "weekly_player_team_record.rds"))
  # dplyr::filter(position == "RB")

# Cumlative Weekly player stats
cuml_player_stats <-  readRDS(here::here("data", "cumulative_player_stats.rds"))

# cuml_player_stats %>% 
#   dplyr::filter(position == "RB") %>% 
#   dplyr::group_by(season, week, team) %>% 
#   slice(1)

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


    

