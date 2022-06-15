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

stats <- readRDS(here::here("data", "weekly_player.rds"))

# Team win/loss
team_records <- readRDS(here::here("data", "team_records2.rds"))

rosters <- readRDS(here::here("data", "rosters.rds"))

week_stats <- readRDS(here::here("data", "weekly_player_team_record.rds")) %>% 
  dplyr::filter(position == "RB")
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
  dplyr::filter(season == 2021, week %in% c(1, 2, 3, 4))
cumulative_mean <- function(x) {
  cummean <- (cumsum(x) / seq_along(x))
  return(cummean)
}
cuml_finish <-
  week_finish %>% 
  dplyr::select(season:player_name, carries, rushing_yards, rushing_epa, targets) %>% 
  # dplyr::filter(carries > 0)
  na.omit() %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::group_by(season, player_id) %>% 
  dplyr::arrange(week, .by_group = T) %>% 
  mutate(across(c(carries:targets), ~ lag(cumulative_mean(.x)), .names = "{col}_cuml")) 
  dplyr::mutate(
    # rushing_yards_cuml = cumsum(rushing_yards)
    rushing_yards_cuml = lag(cumulative_mean(rushing_yards)),
    targets_cuml       = lag(cumulative_mean(targets))
    # rushing_yards_cuml  = cumsum(rushing_yards) / seq_along(rushing_yards),
    # lag_rushing_yards   = lag(rushing_yards_cuml)
  )

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