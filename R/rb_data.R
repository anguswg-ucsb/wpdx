# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR & Tidy Tuesday 2012-05-04 Steam Games dataset

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)
library(corrplot)
library(plotly)
library(viridis)

source("utils/utils.R")

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

fp_df <- readRDS(here::here("data", "fp_model_data.rds"))

# ***************************
# ---- RB data and stats ----
# ***************************


# ***********************
# ---- Win totals QB ----
# ***********************

# season total wins
win_total  <- 
  team_records %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    win  = sum(win, na.rm = T)
  )

# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************

week_finish <- 
  week_stats %>% 
  dplyr::group_by(season, week) %>% 
  dplyr::mutate(
    fp_hppr   = fantasy_points + (receptions*0.5),
    fp_rank   = rank(-fp_hppr,   ties.method = "min"),
    fp_finish = case_when(
      fp_rank <= 5 ~ "top_5",
      TRUE         ~ "not_top_5"
    )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::relocate(season, week, team, win, fp_hppr, fp_rank, fp_finish) %>% 
  dplyr::filter(season == 2021, week %in% c(1, 2, 3, 4)) %>% 
  dplyr::select(season, week, team, player_name,player_id, fp_hppr, fp_rank, fp_finish, carries:receiving_2pt_conversions) %>% 
  dplyr::left_join(
    dplyr::select(team_records, season, team, week, win_pct),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::relocate(season, week, team, player_name, player_id, fp_hppr, fp_rank, fp_finish, win_pct) 

# Cumalive stats
cuml_finish <-
  week_finish %>% 
  dplyr::select(season, week, team, player_name,player_id, fp_hppr, fp_rank, fp_finish,win_pct, carries:receiving_2pt_conversions) %>% 
  # dplyr::filter(carries > 0)
  na.omit() %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::group_by(season, player_id) %>% 
  dplyr::arrange(week, .by_group = T) %>% 
  dplyr::mutate(across(c(carries:receiving_2pt_conversions), ~ lag(cumulative_mean(.x)), .names = "{col}")) %>% 
  dplyr::mutate(
    yards_pc = rushing_yards/carries
  )
na.omit(cuml_finish)
cuml_finish
# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************

# subset roster columns 
rost <- 
  rosters %>% 
  dplyr::ungroup() %>%  
  dplyr::select(season, team, player_id, position)

# subset win loss columns 
wl <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, team, win)

# Win loss. passing stats, 
wl_stats <- 
  qb_stats %>% 
  dplyr::left_join(
    rost, 
    by = c("season", "team", "player_id")
  ) %>% 
  dplyr::left_join(
    wl,
    by = c("team", "season", "week")
  ) %>% 
  dplyr::relocate(season, week, team, win, player_id, player_name, position)

saveRDS(wl_stats, here::here("data", "weekly_player_team_record.rds"))
rm(stats)
# **********************
# ---- QB EPA stats ----
# **********************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
stats <- lapply(seasons_lst, FUN = function(x) {
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

saveRDS(team_records, here::here("data", "team_records2.rds"))

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
# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************

# subset roster columns 
rost <- 
  rosters %>% 
  dplyr::ungroup() %>%  
  dplyr::select(season, team, player_id, position)

# subset win loss columns 
wl <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::select(season, week, team, win)

# Win loss. passing stats, 
wl_stats <- 
  qb_stats %>% 
  dplyr::left_join(
    rost, 
    by = c("season", "team", "player_id")
  ) %>% 
  dplyr::left_join(
    wl,
    by = c("team", "season", "week")
  ) %>% 
  dplyr::relocate(season, week, team, win, player_id, player_name, position)

saveRDS(wl_stats, here::here("data", "weekly_player_team_record.rds"))
rm(stats)
# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************