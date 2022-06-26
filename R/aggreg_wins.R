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
# Snap % # Data dictionary
desc <- nflfastR::field_descriptions

# unique seasons
seasons_lst <- 1999:2021
# % RB carries
# Yards per route run 
# Yard per carry
# Number of top 5 finishes so far

# **********************
# ---- Player stats ----
# **********************



# ---- Team pbp ----
# ******************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
pbp_stats <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Retrieving {x} weekly team pbp stats...")
  pbp <- nflfastR::load_pbp(x)
}
) 

# saveRDS(qb_stats, here::here("data", "weekly_player.rds"))

# ******************************
# ---- Team offensive stats ----
# ******************************


# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
off_stats <- lapply(pbp_stats, FUN = function(x) {
  
  offense <- get_offense(x)

}
) 
off_df <- dplyr::bind_rows(off_stats)

saveRDS(off_df, here::here("data", "offensive.rds"))

# ******************************
# ---- Team defensive stats ----
# ******************************

# load QB stats and calculate game EPA, cumalitve EPA, EPA per play
def_stats <- lapply(pbp_stats, FUN = function(x) {
  
  defense <- get_defense(x)
  
}
) 
def_df <- dplyr::bind_rows(def_stats)
saveRDS(def_df, here::here("data", "defensive.rds"))

 # **********************
# ---- Team records ----
# **********************
rm(pbp)
# pull rosters for every year
team_records <- lapply(seasons_lst, FUN = function(x) {
  
  logger::log_info("Season PBP: {x}")
  
  schedule <- nflfastR::fast_scraper_schedules(x) %>% 
    get_schedule()
  }
) %>%
  dplyr::bind_rows()

saveRDS(team_records, here::here("data", "wins.rds"))

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