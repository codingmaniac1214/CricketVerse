#---- fetch cricsheet ----
library(cricketdata)
library(tidyverse)

tournaments <- cricsheet_codes
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma")) |>
  pull(code)


# d <- tibble()
# for (i in mens_t20_tournaments){
#   x <- fetch_cricsheet(type = "bbb",
#                        gender = "male",
#                        competition = i) |>
#     mutate(tournament = paste0(i))
#   
#   d <- rbind(d, x)
# }

# read the rds file
d <- read_rds("mens_t20_bbb_data")
# if you use the rds file in the app (if that is possible), then you would have to update it constantly
# the alternative is to minimise the amount of loading that needs to happen by fetching all the data at the start but then you're fetching a lot of data that will get thrown out immediately after spending the time getting it

# filter by striker
player_data <- d |>
  filter(striker == "MEK Hussey")

# split
y <- split(player_data, f = player_data$match_id)

find_bbb_male <- function(player_name){
  innings_tibble <- tibble()
  
  for (i in mens_t20_tournaments){
    x <- fetch_cricsheet(type = "bbb",
                         gender = "male",
                         competition = i) |>
      filter(striker == player_name) |>
      mutate(tournament = paste0(i))
    
    innings_tibble <- rbind(innings_tibble, x)
  }
  
  y <- split(innings_tibble, f = innings_tibble$match_id)
  
  return(y)
}
