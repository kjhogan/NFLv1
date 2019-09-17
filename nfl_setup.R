
setup_NFL <- function(install_flag = "n", load_flag = "n" ) {
  
  if(install_flag == "y"){
     install.packages("tidyverse")
     install.packages("dplyr")
      install.packages("na.tools")
     install.packages("ggimage")
     install.packages("webshot")
  }

  if(load_flag == "y"){
     library(tidyverse)
     library(dplyr)
     library(na.tools)
     library(ggimage)
     library(gt) # beautiful tables
     library(DT) # beautiful interactive tables
     library(ggthemes) # custom pre-built themes
     library(bbplot) # more themes
     library(teamcolors) # NFL team colors and logos
     library(ggforce) # better annotations
     library(ggridges) # many distributions at once
     library(ggrepel) # better labels
     library(ggbeeswarm) # beeswarm plots
     library(extrafont) # for extra fonts
    library(glue)
    library(webshot)
    library(nflscrapR)

    pbp <- map_dfr(2009:2018, function(x){
      p <- read_csv(url(glue("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_{x}.csv")))
      return(p)
    })
    

  }

  
  pbp_rp <- pbp %>% 
    filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")
  
  pbp_rp <- pbp_rp %>%
    mutate(
      pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
      rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
      success = ifelse(epa>0, 1 , 0)
    ) 
  pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)
  nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
  chart_data <- pbp_rp %>%
    filter(pass==1) %>%
    group_by(posteam) %>%
    summarise(
      num_db = n(),
      epa_per_db = sum(epa) / num_db,
      success_rate = sum(epa > 0) / num_db
    )
  chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))
  
  chart %>%
    ggplot(aes(x = success_rate, y = epa_per_db)) +
    geom_image(aes(image = url), size = 0.05) +
    labs(x = "Success rate",
         y = "EPA per play",
         caption = "Data from nflscrapR",
         title = "Dropback success rate & EPA/play",
         subtitle = "2018") +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(size = 12))
  
}
