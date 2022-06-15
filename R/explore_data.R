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
url <- "https://es.wikipedia.org/wiki/%C3%8Dndice_de_desarrollo_humano"
# we save in the variable url the website url.
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
#We create a function with read_html to read the web page.
pagina %>%  
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_table(fill=T) -> x
#We save it in a CSV.
View(x)
#Look at the table if is correct.
write.csv(x, "mis_datos_wikipedia.csv")
# ---- Scrape DVOA ----
pagina <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
html <- read_html("https://coinmarketcap.com/historical/20141026/")
tables <- html_nodes(html, "table")
html_table(tables[3], fill = TRUE)
url <- paste0("http://www.speech.cs.cmu.edu/cgi-bin/cmudict?in=happy&stress=-s")
h <- handle(url)
res <- GET(url)
res$headers

RSelenium::rsDriver(port = 4445L, browser = "chrome", chromever = "102.0.5005.61")
file.path(find.package("RSelenium"), "examples/serverUtils")
require(RSelenium)
remDr <- remoteDriver()
library(XML)
url <- "https://www.footballoutsiders.com/stats/nfl/historical-lookup-by-week/2021/1/overall"
# url <- "https://www.opencodez.com/"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
xpathSApply(parsed_doc, path = rel_path)
abs_path <- "/html/body/div[1]/div[1]/div/div[2]/div/main/div[2]/div[2]/div/div[4]/div/table"
rel_path <- '//*[@id="block-fo-front-content"]/div/div[4]/div/table'
remDr$navigate("some url")
    # url <- "https://www.footballoutsiders.com/stats/nfl/team-efficiency/2021/regular"
    # url <- "https://www.footballoutsiders.com/dvoa-analysis/2019/week-8-dvoa-ratings"
    url <- "https://www.footballoutsiders.com/stats/nfl/historical-lookup-by-week/2012/1/overall"
    binman::list_versions("chromedriver")
    fo_page <- read_html(url)
    
    fo_page[[1]]
    fo_nodes <- 
      fo_page %>%  
      html_nodes("table")
    fo_table1 <- html_table(fo_nodes, fill = F, convert = T)
    
    fo_length1 <- length(fo_nodes)
    fo_length2 <- length(fo_nodes) - 1
    
    fo_nodes[[fo_length]]
    # %>% 
    #   .[[4]]
    
    # lapply(fo_nodes, html_table, fill = TRUE)
    fo_table1 <- html_table(fo_nodes[[fo_length1]], fill = TRUE, convert = F)
    
    fo_table2 <- html_table(fo_nodes[[fo_length2]], fill = TRUE, convert = F)
    
  .[[1]] %>% 
  html_table(convert = F)
fo_table$doc[[1]]
fo_page$doc
fo_page = GET("https://www.footballoutsiders.com/stats/nfl/team-efficiency/2021/regular?check_logged_in=1") %>% 
  content(as='raw') %>% 
  fromJSON()
fo_page$all_headers
fo_page
library(RSelenium)
driver = rsDriver(browser = c("firefox"))

remDr <- driver[["client"]]
# Naigate to website
remDr$navigate("https://www.bankofengland.co.uk/boeapps/database/fromshowcolumns.asp?Travel=NIxSTxTIxSUx&FromSeries=1&ToSeries=50&DAT=RNG&FD=1&FM=Jan&FY=2009&TD=30&TM=Mar&TY=2020&FNY=&CSVF=TT&html.x=91&html.y=29&C=IIN&Filter=N#")
#Download the CSV file
button_element <- remDr$findElement(using = 'xpath', value = '//*[@id="stats-table_wrapper"]/div[1]/a[2]')
button_element$clickElement()
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
team_records <-  readRDS(here::here("data", "team_records2.rds")) %>% 
  dplyr::filter(team != "")

# season total wins
win_total  <- 
  team_records %>% 
  dplyr::group_by(season, team) %>% 
  dplyr::summarise(
    win  = sum(win, na.rm = T)
  )

football     <- readRDS(here::here("data", "weekly_player_team_record.rds"))

play_stats <- 
  football %>% 
  dplyr::group_by(season, week, team) %>% 
  # dplyr::mutate(
  #   fumbles = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost 
  # )
  dplyr::summarise(
    across(c(completions:fantasy_points_ppr), sum, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(    
    fumbles   = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost,
    turnovers = fumbles + interceptions
  ) %>% 
  dplyr::left_join(
    dplyr::select(team_records, season, team, home_away, week,max_score_diff, min_score_diff,roof, surface, win),
    by = c("season", "week", "team")
  ) %>% 
  dplyr::select(
    -rushing_fumbles_lost, -sack_fumbles_lost, -receiving_fumbles_lost,
    -fantasy_points, -fantasy_points_ppr
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
  dplyr::select(-surface)

saveRDS(play_stats, here::here("data", "nfl_wins.rds"))

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
# **************************************************
# ---- Join QB Adv stats w/ standard stats data ----
# **************************************************

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


    

