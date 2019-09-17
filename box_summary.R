game_summary <- function(season = 2018, home = "PIT", away = "NE", type = "reg") {
  
  ids <- nflscrapR::scrape_game_ids(season, teams = home, type = type)
  id <- ids %>% dplyr::filter(away_team == away) %>% dplyr::pull(game_id)
  
  
  pbp_data <- nflscrapR::scrape_json_play_by_play(id)
  
  # clean it up
  pbp <- pbp_data %>%
    filter(!is_na(epa), !is_na(posteam), play_type == "no_play" | play_type == "pass" | play_type == "run") %>%
    mutate(
      pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
      rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
      success = ifelse(epa > 0, 1, 0),
      passer_player_name = ifelse(play_type == "no_play" & pass == 1,
                                  str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                  passer_player_name
      ),
      receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"),
                                    str_extract(
                                      desc,
                                      "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"
                                    ),
                                    receiver_player_name
      ),
      rusher_player_name = ifelse(play_type == "no_play" & rush == 1,
                                  str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                  rusher_player_name
      ),
      name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
      yards_gained = ifelse(play_type == "no_play", NA, yards_gained),
      play = 1
    ) %>%
    filter(pass == 1 | rush == 1)
  
  home <- pbp %>% select(home_team, away_team) %>% slice(1) %>% pull(home_team)
  away <- pbp %>% select(home_team, away_team) %>% slice(1) %>% pull(away_team)
  
  # do stuff for the team summary table
  all <- pbp %>%
    group_by(posteam) %>%
    summarize(
      epa = mean(epa), success = mean(success), p = mean(pass), play = n()
    ) %>%
    mutate(rowname = "All plays", type = 1)
  
  early <- pbp %>%
    filter(down == 1 | down == 2) %>%
    group_by(posteam) %>%
    summarize(
      epa = mean(epa), success = mean(success), p = mean(pass), play = n()
    ) %>%
    mutate(rowname = "Early downs (1st & 2nd)", type = 4)
  
  earlyr <- pbp %>%
    filter((down == 1 | down == 2) & rush == 1) %>%
    group_by(posteam) %>%
    summarize(
      epa = mean(epa), success = mean(success), p = mean(pass), play = n()
    ) %>%
    mutate(rowname = "Early rush", type = 5)
  
  earlyp <- pbp %>%
    filter((down == 1 | down == 2) & pass == 1) %>%
    group_by(posteam) %>%
    summarize(
      epa = mean(epa), success = mean(success), p = mean(pass), play = n()
    ) %>%
    mutate(rowname = "Early pass", type = 6)
  
  late <- pbp %>%
    filter(down == 3 | down == 4) %>%
    group_by(posteam) %>%
    summarize(
      epa = mean(epa), success = mean(success), p = mean(pass), play = n()
    ) %>%
    mutate(rowname = "Late downs (3rd & 4th)", type = 7)
  
  type <- pbp %>%
    group_by(posteam, pass) %>%
    summarize(
      epa = mean(epa), success = mean(success), p = mean(pass), play = n()
    ) %>%
    mutate(rowname = if_else(pass == 1, "Pass", "Rush"), type = 2)
  
  bound <- bind_rows(all, early, earlyr, earlyp, late, type) %>%
    mutate(home = ifelse(posteam == home, 1, 0), p = round(100 * p), epa = round(epa, digits = 2), success = round(success, digits = 2)) %>%
    arrange(home, type) %>%
    select(-pass, -type, -home)
  
  # team summary table
  table <- bound %>%
    select(posteam, rowname, epa, success, play) %>%
    group_by(posteam) %>%
    gt() %>%
    cols_label(
      epa = md("**EPA/<br>play**"), success = md("**Success<br>rate**"), play = md("**Plays**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      source_note = "Table: @benbbaldwin | Data: @nflscrapR"
    ) %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ), locations = cells_group(groups = TRUE)
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic", align = "center")
      ),
      locations = cells_stub(rows = c(2, 3, 9, 10, 5, 6, 12, 13))
    )
  
  ##### do stuff for player summary table
  
  rushers <- pbp %>%
    filter(rush == 1) %>%
    group_by(rusher_player_name, posteam) %>%
    summarize(
      tot_epa = sum(epa), epa = mean(epa), success = mean(success), play = n()
    ) %>%
    mutate(rowname = "Rush attempts", type = 1, p = "Rush attempts", rowname = rusher_player_name) %>%
    ungroup()
  
  receivers <- pbp %>%
    filter(rush == 0 & !is.na(receiver_player_name)) %>%
    group_by(receiver_player_name, posteam) %>%
    summarize(
      tot_epa = sum(epa), epa = mean(epa), success = mean(success), play = n()
    ) %>%
    mutate(rowname = "Targets", type = 2, p = "Targets", rowname = receiver_player_name) %>%
    ungroup()
  
  passers <- pbp %>%
    filter(rush == 0 & !is.na(name)) %>%
    group_by(name, posteam) %>%
    summarize(
      tot_epa = sum(epa), epa = mean(epa), success = mean(success), play = n()
    ) %>%
    mutate(rowname = "Dropbacks", type = 0, p = "Dropbacks", rowname = name) %>%
    ungroup()
  
  
  rp <- bind_rows(passers, rushers, receivers) %>%
    mutate(home = ifelse(posteam == home, 1, 0), epa = round(epa, digits = 2), tot_epa = round(tot_epa, digits = 1), success = round(success, digits = 2)) %>%
    arrange(type, home, desc(play)) %>%
    select(-type, -rusher_player_name, -receiver_player_name, -name)
  
  # player summary as one big table
  t2 <- rp %>%
    select(posteam, rowname, epa, tot_epa, success, play, p) %>%
    group_by(p) %>%
    gt() %>%
    cols_label(
      posteam = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), epa = md("**EPA/<br>play**"), tot_epa = md("**Total<br>EPA**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      source_note = "Table: @benbbaldwin | Data: @nflscrapR"
    ) %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ), locations = cells_group(groups = TRUE)
    )
  
  # player summary for away team
  t2a <- rp %>%
    filter(home == 0) %>%
    select(posteam, rowname, epa, tot_epa, success, play, p) %>%
    group_by(p) %>%
    gt() %>%
    cols_label(
      posteam = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), epa = md("**EPA/<br>play**"), tot_epa = md("**Total<br>EPA**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      source_note = "Table: @benbbaldwin | Data: @nflscrapR"
    ) %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ), locations = cells_group(groups = TRUE)
    )
  
  # player summary for hom team
  t2h <- rp %>%
    filter(home == 1) %>%
    select(posteam, rowname, epa, tot_epa, success, play, p) %>%
    group_by(p) %>%
    gt() %>%
    cols_label(
      posteam = md("**Team**"), success = md("**Success<br>rate**"), play = md("**Plays**"), epa = md("**EPA/<br>play**"), tot_epa = md("**Total<br>EPA**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(
      source_note = "Table: @benbbaldwin | Data: @nflscrapR"
    ) %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ), locations = cells_group(groups = TRUE)
    )
  
  
  # export all the tables
  
  # team summary
  gtsave(table, 
         filename =  glue::glue("{home}_{away}_{season}_game_summary.png"))
  
  # player summary
  gtsave(t2, 
         filename =  glue::glue("{home}_{away}_{season}_game_players.png"))
  
  # away player summary
  gtsave(t2a, 
         filename =  glue::glue("{home}_{away}_{season}_game_players_away.png"))
  
  # home player summary
  gtsave(t2h, 
         filename =  glue::glue("{home}_{away}_{season}_game_players_home.png"))
}