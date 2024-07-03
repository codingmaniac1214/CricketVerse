#----- Update batR data -----
# last updated: 4th May 2023
library(cricketdata)
library(dplyr)

tournaments <- cricsheet_codes

#---- save men's ball-by-ball data ----
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma", "ssm")) |>
  pull(code)

male_data <- tibble()
for(i in mens_t20_tournaments){
  x <- fetch_cricsheet(type = "bbb",
                       gender = "male",
                       competition = i) |>
    mutate(tournament = paste0(i))
  
  male_data <- rbind(male_data, x) 
}
saveRDS(male_data, file = "data/mens_ball_by_ball_data.rds")

#---- save women's ball-by-ball data ----
womens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "blz", "cec", "frb", "wbb", "wcl", "wsl", "wtc")) |>
  pull(code)

female_data <- tibble()
for(i in womens_t20_tournaments){
  x <- fetch_cricsheet(type = "bbb",
                       gender = "female",
                       competition = i) |>
    mutate(tournament = paste0(i))
  
  female_data <- rbind(female_data, x)
}
saveRDS(female_data, file = "data/womens_ball_by_ball_data.rds")
