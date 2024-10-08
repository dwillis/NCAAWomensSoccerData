library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)

urls <- read_csv("url_csvs/ncaa_womens_soccer_teamurls_2024.csv") %>% pull(2)
season = "2024"

root_url <- "https://stats.ncaa.org"
playerstatstibble = tibble()

playerstatsfilename <- paste0("data/ncaa_womens_soccer_playerstats_", season, ".csv")

for (i in urls){
  
  schoolpage <- i %>% read_html()
  
  schoolfull <- schoolpage %>% html_nodes(xpath = '/html/body/div[2]/div/div/div/div/div/div[1]/a') %>% html_text()
#  schoolfull <- schoolpage %>% html_nodes(xpath = '//*[@id="contentarea"]/fieldset[1]/legend/a[1]') %>% html_text()
  
  playerstats <- schoolpage %>% html_nodes(xpath = '//*[@id="stat_grid"]') %>% html_table()
  
  playerstats <- playerstats[[1]] %>% filter(Player != "TEAM" & Player != "Totals" & Player != "Opponent Totals") %>% mutate(RosterName = Player) %>% separate(Player, into=c("LastName", "FirstName"), sep=",") %>% mutate(FullName = paste(FirstName, LastName, sep=" ")) %>% mutate(Team = schoolfull[[1]], Season=season) %>% clean_names() %>% select(season, team, full_name, roster_name, first_name, last_name, yr, pos, everything()) %>% mutate_at(vars(-season, -team, -full_name, -roster_name, -first_name, -last_name, -yr, -pos), ~str_replace(., ",", "")) %>%  mutate_at(vars(-season, -team, -full_name, -roster_name, -first_name, -last_name, -yr, -pos), as.numeric)
  
  message <- paste0("Fetching ", schoolfull)
  
  print(message)
  
  tryCatch(playerstatstibble <- bind_rows(playerstatstibble, playerstats),
           error = function(e){NA})
  
  Sys.sleep(1)
}

playerstatstibble <- playerstatstibble %>% remove_empty(which="rows")

write_csv(playerstatstibble, playerstatsfilename)