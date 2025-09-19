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
  
  # Extract school name from card header - fixed extraction
  card_header <- schoolpage %>% html_nodes(".card-header") %>% html_text() %>% .[1]
  if(!is.na(card_header) && length(card_header) > 0) {
    # Extract team name (everything before the record in parentheses)
    # Don't remove any leading text - keep the full team name
    schoolfull <- str_extract(card_header, ".*?(?=\\s*\\([0-9]+-[0-9]+-?[0-9]*\\))") %>% 
      str_trim()
    
    # If extraction failed, try alternative method
    if(is.na(schoolfull) || schoolfull == "") {
      # Look for team name in the link text
      team_link <- schoolpage %>% html_nodes(".card-header a") %>% html_text() %>% .[1]
      if(!is.na(team_link)) {
        schoolfull <- str_extract(team_link, ".*?(?=\\s*\\([0-9]+-[0-9]+-?[0-9]*\\))") %>% str_trim()
      } else {
        schoolfull <- "Unknown"
      }
    }
  } else {
    schoolfull <- "Unknown"
  }
  
  # Extract team_id from the ranking summary link
  ranking_link <- schoolpage %>% 
    html_nodes("a[href*='ranking_summary']") %>% 
    html_attr("href") %>% 
    .[1]
  
  team_id <- NA
  if(!is.na(ranking_link)) {
    team_id_match <- str_extract(ranking_link, "org_id=([0-9]+)")
    if(!is.na(team_id_match)) {
      team_id <- str_extract(team_id_match, "[0-9]+") %>% as.numeric()
    }
  }
  
  # Extract the statistics table
  playerstats_raw <- schoolpage %>% html_nodes(xpath = '//*[@id="stat_grid"]') %>% html_table()
  
  if(length(playerstats_raw) > 0) {
    playerstats <- playerstats_raw[[1]]
    
    # Extract data-order attributes for proper name parsing
    player_rows <- schoolpage %>% html_nodes('#stat_grid tbody tr')
    
    # Get the data-order attributes from the Player column (2nd td in each row)
    data_order_names <- player_rows %>% 
      html_nodes('td:nth-child(2)') %>% 
      html_attr('data-order')
    
    # Filter out team totals and opponents from both datasets
    valid_rows <- !str_detect(playerstats$Player, "^(TEAM|Totals|Opponent Totals)$")
    playerstats <- playerstats[valid_rows, ]
    data_order_names <- data_order_names[valid_rows]
    
    # Process the data
    playerstats <- playerstats %>%
      mutate(
        # Use the visible player name as roster_name and full_name
        full_name = Player %>% str_trim(),
        
        # Parse the data-order attribute for proper first/last name separation
        data_order_name = data_order_names,
        
        # Extract last and first names from data-order (format: "LastName,FirstName")
        last_name = case_when(
          !is.na(data_order_name) & str_detect(data_order_name, ",") ~ 
            str_extract(data_order_name, "^[^,]+") %>% str_trim(),
          TRUE ~ str_extract(Player, "\\S+$") %>% str_trim()  # fallback to last word
        ),
        first_name = case_when(
          !is.na(data_order_name) & str_detect(data_order_name, ",") ~ 
            str_extract(data_order_name, "(?<=,).*$") %>% str_trim(),
          TRUE ~ str_extract(Player, "^\\S+") %>% str_trim()  # fallback to first word
        ),
        
        team = schoolfull,
        team_id = team_id,
        season = as.numeric(season)
      ) %>%
      # Clean column names - this handles most of the renaming automatically
      clean_names() %>%
      # Rename the player column to roster_name
      rename(roster_name = player) %>%
      # Select and reorder columns to match expected CSV structure
      select(
        season, team, team_id, full_name, roster_name, first_name, last_name, 
        yr, pos, number, ht, gp, gs, minutes, goals, assists, points, 
        sh_att, so_g, fouls, red_cards, yellow_cards, corners, pk, pk_att, gwg
      ) %>%
      # Clean numeric columns - handle time format properly
      mutate(
        # Convert time format (MM:SS) to decimal minutes
        minutes = case_when(
          str_detect(as.character(minutes), ":") ~ {
            time_parts <- str_split(as.character(minutes), ":")
            map_dbl(time_parts, ~as.numeric(.x[1]) + as.numeric(.x[2])/60)
          },
          TRUE ~ as.numeric(str_replace_all(as.character(minutes), "[,]", ""))
        ),
        # Convert other numeric columns normally
        across(c(number, ht, gp, gs, goals, assists, points, 
                 sh_att, so_g, fouls, red_cards, yellow_cards, corners, 
                 pk, pk_att, gwg), ~as.numeric(str_replace_all(as.character(.), "[,-]", "")))
      )
    
    message <- paste0("Fetching ", schoolfull)
    print(message)
    
    tryCatch({
      # Explicitly ensure team_id is included
      playerstats$team_id <- team_id
      playerstatstibble <- bind_rows(playerstatstibble, playerstats)
    }, error = function(e) {
      print(paste("Error processing", schoolfull, ":", e$message))
    })
  }
  
  Sys.sleep(1)
}

# Remove empty rows and write to CSV
playerstatstibble <- playerstatstibble %>% 
  remove_empty(which = "rows") %>%
  filter(!is.na(full_name))

write_csv(playerstatstibble, playerstatsfilename)