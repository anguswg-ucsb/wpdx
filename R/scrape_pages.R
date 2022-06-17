# Angus Watters 
# Exploratory Data Analysis
# NFL Data from nflFastR & Tidy Tuesday 2012-05-04 Steam Games dataset

rm(list = ls())

# Get the Data
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
require(RSelenium)
library(XML)
library(janitor)

source("utils/utils.R")

data_path  <-  here::here("data")

# ********************************
# ---- Scrape Pro Footbal Ref ----
# ********************************

season_years <- 1999:2021
# season_year <- 2021
def_ranks <- lapply(season_years, FUN = function(x){
  pgs <- scrape_pfr(x)
}) %>% 
  dplyr::bind_rows()

saveRDS(def_ranks, here::here("data", "rush_defense.rds"))
# ********************************
# ---- Scrape Pro Footbal Ref ----
# ********************************

url <- "https://www.pro-football-reference.com/years/2021/fantasy-points-against-RB.htm"

pfr_page <- read_html(url)

pfr_nodes <- 
  pfr_page %>%  
  html_nodes("table")

# Scrape table from Pro football ref
pfr_table <- html_table(
  pfr_nodes[[1]],
  header  = T,
  fill    = T, 
  convert = T) %>% 
setNames(
  c("team", "games", "rush_attempts", "rush_yards", "rush_tds", "receiving_targets",
    "receiving_rec", "receiving_yards", "receiving_tds",  "fantasy_points", "fantasy_points_dk",
    "fantasy_points_fd",  "fppg", "fppg_dk", "fppg_fd")
  ) %>% 
  slice(-1) %>% 
  dplyr::select(team:receiving_tds, fp_hppr = fppg_fd) %>% 
  dplyr::mutate(across(c(rush_attempts:fp_hppr), as.numeric))

# Rank offense and defense 
pfr_rank <- 
  pfr_table %>% 
  dplyr::mutate(
    rush_rank = rank(rush_yards, ties.method = "min"),
    rec_rank  = rank(receiving_yards,  ties.method = "min")
    )
# https://www.pro-football-reference.com/years/2021/opp.htm#rushing
season_year <- 2021
url         <- paste0("https://www.pro-football-reference.com/years/", season_year, "/opp.htm")
url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fyears%2F2021%2Fopp.htm&div=div_rushing"

# ****************************
# ---- Team defense stats ----
# ****************************

season_years <- 1999:2021
# season_year <- 2021
def_ranks <- lapply(season_years, FUN = function(x){
  pgs <- scrape_pfr(x)
}) %>% 
  dplyr::bind_rows()


# url         <- paste0("https://www.pro-football-reference.com/years/", season_year, "/opp.htm")
url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fyears%2F2021%2Fopp.htm&div=div_rushing"
url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fyears%2F1999%2Fopp.htm&div=div_rushing"
rushd_page <- read_html(url)
year <- 1999


rushd_nodes <- 
  rushd_page %>%  
  html_nodes("table")
rushd_nodes[[2]]
# Scrape table from Pro football ref
rushd_table <- html_table(
  rushd_nodes[[1]],
  header  = T,
  fill    = T, 
  convert = T
  ) 
pfr_table2 <- pfr_table %>% row_to_names(row_number = 1)

pfr_table2 <- 
  pfr_table %>% 
  t() %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)
column_to_rownames(pfr_table, "A") %>% 
# *****************
# ---- FO DVOA ----
# *****************

RSelenium::rsDriver(port = 4445L, browser = "chrome", chromever = "102.0.5005.61")
file.path(find.package("RSelenium"), "examples/serverUtils")
remDr <- remoteDriver()
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

driver = rsDriver(browser = c("firefox"))

remDr <- driver[["client"]]
# Naigate to website
remDr$navigate("https://www.bankofengland.co.uk/boeapps/database/fromshowcolumns.asp?Travel=NIxSTxTIxSUx&FromSeries=1&ToSeries=50&DAT=RNG&FD=1&FM=Jan&FY=2009&TD=30&TM=Mar&TY=2020&FNY=&CSVF=TT&html.x=91&html.y=29&C=IIN&Filter=N#")
#Download the CSV file
button_element <- remDr$findElement(using = 'xpath', value = '//*[@id="stats-table_wrapper"]/div[1]/a[2]')
button_element$clickElement()
