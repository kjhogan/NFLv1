get_season_Odds <- function(season_data) {
  dates <- season_data$date %>% unique()
  all_odds <- lapply(dates, oddsScraperNBA)
  beep(8)
  return(all_odds)
}