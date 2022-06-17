# *********************
# ---- Correlation ----
# *********************

# Corelation matrix
mcor <- cor(play_stats[sapply(play_stats,is.numeric)]) %>% 
  reshape2::melt() %>% 
  tibble::tibble() %>% 
  dplyr::mutate(across(where(is.numeric), round, 2))

# Correlation heatmap
cor_plot <- 
  ggplot(mcor1, aes(x = Var1,
                    y = Var2,
                    fill = value)) + 
  geom_tile() +
  theme(axis.text = element_text(angle = 45, vjust = 0.1, hjust = 1)) +
  scale_fill_viridis(discrete = FALSE, direction = 1)

cor_plot

plotly::ggplotly(cor_plot)

# ******************************
# ---- Correlated variables ----
# ******************************

# Select most correlated variables
feat_select <- 
  cor(play_stats[sapply(play_stats,is.numeric)])  %>% 
  reshape2::melt() %>% 
  tibble::tibble() %>% 
  dplyr::mutate(across(where(is.numeric), round, 4)) %>% 
  dplyr::filter(Var2 == "win") %>% 
  dplyr::mutate(
    abs_corr = abs(value)
  ) 
# dplyr::filter(abs_corr >= 0.1)

saveRDS(feat_select, here::here("data", "nfl_feature_selection.rds"))

# ***************
# ---- Plots ----
# ***************

play_stats    <- readRDS(here::here("data", "nfl_wins.rds")) 
team_records  <-  readRDS(here::here("data", "team_records2.rds")) %>% 
  dplyr::filter(team != "")


# season total wins
win_total  <- 
  team_records %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise((across(where(is.numeric), sum, na.rm = T))) %>% 
  dplyr::ungroup() 

# Average wins per season per team
team_wins <- 
  win_total %>% 
  dplyr::group_by(team) %>% 
  dplyr::summarise((across(where(is.numeric), mean, na.rm = T))) %>% 
  dplyr::ungroup() 

ggplot() +
  geom_point(data = team_wins, aes(x = reorder(team, win), y = win))
# Season averages
season_stats <- 
  play_stats %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(across(where(is.numeric), mean))

ggplot() +
  geom_boxplot(data = play_stats, aes(x = factor(season), y = passing_epa, outlier.shape = NA))
# geom_point(data = season_stats, aes(x = season, y = passing_epa))
# *****************************
# ---- Stepwise regression ----
# *****************************

nfl_trim <- 
  play_stats %>% 
  dplyr::select(-week, -team, -season)

lm_fit <- lm(win~., data = nfl_trim)
broom::glance(lm_fit)
summary(lm_fit)
lm_ols <- olsrr::ols_step_forward_p(lm_fit)
lm_ols$model



head(data1)
palette = colorRampPalette(c("green", "white", "red"))(20)
corrplot(mcor)
ggplotly(heatmap(x = mcor, col = palette, symm = T))
# dplyr::left_join(
#   win_total, 
#   by = c("season", "team")
# )

plot(play_stats$turnovers~play_stats$win)

ggplot() +
  # geom_point(data = play_stats, aes(x = week, y = runs)) 
  geom_point(data = play_stats, aes(x = season, y = value, color = name))  +
  facet_wrap(~team)
# coord_flip()
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

# play_stats <- readRDS(here::here("data", "nfl_wins.rds"))

week_stats     <- readRDS(here::here("data", "weekly_player_team_record.rds"))
# QB stats
qb <- 
  football %>% 
  dplyr::filter(position == "QB") 

qb_stats <- 
  qb %>% 
  dplyr::group_by(team, season) %>% 
  dplyr::summarise(
    pass = sum(passing_yards, na.rm = T),
    run  = sum(rushing_yards, na.rm = T)
  ) %>% 
  dplyr::arrange(season) %>% 
  tidyr::pivot_longer(cols = c(run, pass)) %>% 
  dplyr::ungroup()

tmp <- 
  wl_stats %>% 
  dplyr::filter(position == "QB") %>% 
  dplyr::select(-team, -player_id, -player_name, -position) 
# dplyr::mutate(
#   turnover_ratio = (rushing_tds + passing_tds + receiving_tds)/(interceptions + rushing_fumbles_lost),
#   turnover_ratio2 = case_when(
#     is.nan(turnover_ratio ) ~ 1,
#     is.infinite(turnover_ratio) ~ 2,
#     TRUE ~ turnover_ratio
#   )
# )

lm <- lm(win~passing_epa                  , data = tmp)
summary(lm)
tmp_plot <- 
  tmp %>% 
  correlate() %>%    # Create correlation data frame (cor_df)
  # focus(-cyl, -vs, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
  # rearrange() %>%  # rearrange by correlations
  # shave() %>% 
  rplot() + 
  theme(axis.text.x = element_text(angle = 270, vjust = .5))
plotly::ggplotly(tmp_plot)
library(corrr)
x <- datasets::mtcars %>%
  correlate() %>%    # Create correlation data frame (cor_df)
  focus(-cyl, -vs, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
  rearrange() %>%  # rearrange by correlations
  shave()
rplot(x)