# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR & Tidy Tuesday 2012-05-04 Steam Games dataset

rm(list = ls())

# Get the Data
library(tidyverse)
library(janitor)
library(stringr)
library(nflfastR)

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
qb_epa <- lapply(seasons_lst, FUN = function(x) {
  pbp <- nflfastR::load_pbp(x) %>% 
    nflfastR::add_qb_epa() %>% 
    get_qb_stats()
  }
  ) %>%
  dplyr::bind_rows()

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

# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************

# qb_stats <- inner_join(qb_epa,  dplyr::select(stats, ))

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


    

