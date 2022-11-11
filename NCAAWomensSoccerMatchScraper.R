library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)

urls <- read_csv("url_csvs/ncaa_womens_soccer_teamurls_2022.csv") %>% pull(3)

season = "2022"

root_url <- "https://stats.ncaa.org"

matchstatstibble = tibble()

matchstatsfilename <- paste0("data/ncaa_womens_soccer_matchstats_", season, ".csv")

for (i in urls){
  
  schoolpage <- i %>% read_html()
  
  schoolfull <- schoolpage %>% html_nodes(xpath = '//*[@id="contentarea"]/fieldset[1]/legend/a[1]') %>% html_text()
  
  matches <- schoolpage %>% html_nodes(xpath = '//*[@id="game_breakdown_div"]/table') %>% html_table(fill=TRUE)
  
  matches <- matches[[1]] %>% slice(3:n()) %>% 
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    remove_empty(which = c("cols")) %>% 
    mutate_all(na_if,"") %>% 
    fill(c(date, result)) %>% 
    mutate_at(vars(5:26),  replace_na, '0') %>% 
    mutate(date = mdy(date), home_away = case_when(grepl("@",opponent) ~ "Away", TRUE ~ "Home"), opponent = gsub("@ ","",opponent)) %>%
    separate(result, into=c("score", "overtime"), sep = " \\(") %>% 
    separate(score, into=c("team_score", "opponent_score")) %>%
    mutate(outcome = case_when(opponent_score > team_score ~ "Loss", team_score > opponent_score ~ "Win", opponent_score == team_score ~ "Draw")) %>% 
    mutate(team = schoolfull) %>% 
    mutate(overtime = gsub(")", "", overtime)) %>% 
    select(date, team, opponent, home_away, outcome, team_score, opponent_score, overtime, everything()) %>% 
    clean_names() %>% 
    mutate_at(vars(-date, -opponent, -home_away, -outcome, -team), ~str_replace(., "/", "")) %>% 
    mutate_at(vars(-date, -team, -opponent, -home_away, -outcome, -overtime, -goalie_min_plyd), as.numeric)
  
  teamside <- matches %>% filter(opponent != "Defensive Totals")
  
  opponentside <- matches %>% filter(opponent == "Defensive Totals") %>% select(-opponent, -home_away) %>% rename_with(.cols = 8:29, function(x){paste0("defensive_", x)})
  
  joinedmatches <- inner_join(teamside, opponentside, by = c("date", "team", "outcome", "team_score", "opponent_score", "overtime", "games"))
  
  tryCatch(matchstatstibble <- bind_rows(matchstatstibble, joinedmatches),
           error = function(e){NA})
  
  message <- paste0("Adding ", schoolfull)
  
  print(message)
  
  Sys.sleep(1)
}

write_csv(matchstatstibble, matchstatsfilename)
