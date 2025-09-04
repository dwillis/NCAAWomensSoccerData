library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)

urls <- read_csv("url_csvs/ncaa_womens_soccer_teamurls_2025.csv") %>% pull(3)

season = "2025"

root_url <- "https://stats.ncaa.org"

matchstatstibble = tibble()

matchstatsfilename <- paste0("data/ncaa_womens_soccer_matchstats_", season, ".csv")

for (i in urls){

  team_id = str_split(str_split(i, '&', n=3)[[1]][[1]], '=')[[1]][[2]]

  schoolpage <- i %>% read_html()

  schoolfull <- schoolpage %>% html_nodes(xpath = '/html/body/div[2]/div/div/div/div/div/div[1]/a') %>% html_text()

  if(length(schoolfull) == 0) {
    card_text <- schoolpage %>% html_nodes(".card-header") %>% html_text() %>% .[1]
    if(!is.na(card_text)) {
      # Extract team name (everything before the record)
      schoolfull <- str_extract(card_text, ".*?(?=\\s*\\([0-9]+-[0-9]+-?[0-9]*\\))") %>% str_trim()
      if(is.na(schoolfull)) schoolfull <- character(0)
    } else {
      schoolfull <- character(0)
    }
  }

  matches <- schoolpage %>% html_nodes(xpath = '//*[@id="game_breakdown_div"]/table') %>% html_table(fill=TRUE)

  matches <- matches[[1]] %>% slice(3:n()) %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    remove_empty(which = c("cols")) %>%
    mutate_all(na_if,"") %>%
    fill(c(date, result)) %>%
    mutate_at(vars(5:15),  replace_na, '0') %>%
    mutate(date = mdy(date), home_away = case_when(grepl("@",opponent) ~ "Away", TRUE ~ "Home"), opponent = gsub("@ ","",opponent)) %>%
    separate(result, into=c("score", "overtime"), sep = " \\(") %>%
    separate(score, into=c("team_score", "opponent_score")) %>%
    mutate(outcome = case_when(opponent_score > team_score ~ "Loss", team_score > opponent_score ~ "Win", opponent_score == team_score ~ "Draw")) %>%
    mutate(team = schoolfull[[1]]) %>%
    mutate(overtime = gsub(")", "", overtime)) %>%
    select(date, team, opponent, home_away, outcome, team_score, opponent_score, overtime, everything()) %>%
    clean_names() %>%
    mutate_at(vars(-date, -opponent, -home_away, -outcome, -team), ~str_replace(., "/", "")) %>%
    mutate_at(vars(-date, -team, -opponent, -home_away, -outcome, -overtime, -minutes), as.numeric)

  teamside <- matches %>% filter(opponent != "Defensive Totals")

  opponentside <- matches %>% filter(opponent == "Defensive Totals") %>% select(-opponent, -home_away, -overtime, -gp) %>% rename_with(.cols = 6:18, function(x){paste0("defensive_", x)})

  joinedmatches <- inner_join(teamside, opponentside, by = c("date", "team", "outcome", "team_score", "opponent_score"))

  joinedmatches <- joinedmatches %>% add_column(team_id = team_id)

  # grab opponent IDs - the one issue here is that if a team plays an opponent that isn't linked, this won't work and the team's matches will not have any opponent_id values
  #opponent_ids <- schoolpage %>% html_nodes("a") %>% html_attr("href") %>% as_tibble() %>% filter(str_detect(value, "/teams/")) %>% filter(!str_detect(value, paste0("/", team_id, "/"))) %>% separate(value, into=c('blank', 'team', 'opponent_id', 'season_id'), sep = '/') %>% select(opponent_id)

  #tryCatch(joinedmatches <- bind_cols(joinedmatches, opponent_ids),
  #         error = function(e){NA})

  tryCatch(matchstatstibble <- bind_rows(matchstatstibble, joinedmatches),
           error = function(e){NA})

  message <- paste0("Adding ", schoolfull)

  print(message)

  Sys.sleep(1)
}

write_csv(matchstatstibble %>% distinct(), matchstatsfilename)
