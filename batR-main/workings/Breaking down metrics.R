#----- Breaking down metrics -----
#---- libraries ----
library(cricketdata)
library(tidyverse)
library(fmsb)

#---- functions ----
source("app/functions/balls_per_boundary.R")
source("app/functions/career_bbb.R")
source("app/functions/career_mean_bbb.R")
source("app/functions/dot_ball_percentage.R")
source("app/functions/find_bbb.R")
source("app/functions/innings_table.R")
source("app/functions/spider_plot.R")
source("app/functions/tournament_mean_bbb.R")

#---- data ----
tournaments <- cricsheet_codes
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma")) |>
  pull(code)

mens_t20_data <- read_rds("app/data/mens_ball_by_ball_data.rds")

# player data
conway_innings_list <- find_bbb("DP Conway", "male", mens_t20_data)
conway_bbb_tibble <- career_bbb(conway_innings_list)
conway_innings_table <- innings_table(conway_bbb_tibble)


#===== by tournament =====
#---- metrics by tournament ----
# split the ball by ball tibble into a list of tibbles differentiated by tournament. Each row in the tibble is an innings in that tournament
innings_by_tournament <- split(conway_bbb_tibble, conway_bbb_tibble$tournament)

# take one of the tournaments and apply the functions to calculate metrics
ctc <- innings_by_tournament[[1]]
# innings
innings_n <- nrow(ctc)
# runs scored, SR & balls faced
ctc_innings_table <- innings_table(ctc)
mean_runs <- round(mean(ctc_innings_table$`total scored`))
median_runs <- round(median(ctc_innings_table$`total scored`))
mean_SR <- round(mean(ctc_innings_table$SR))
median_SR <- round(median(ctc_innings_table$SR))
mean_bf <- round(mean(ctc_innings_table$`balls faced`))
median_bf <- round(median(ctc_innings_table$`balls faced`))
# dot ball percentage
dbp <- dot_ball_percentage(ctc)
# balls per boundary
bpb <- balls_per_boundary(ctc)

ctc_metrics <- tibble(innings_n, dbp, bpb,
                      mean_runs, median_runs,
                      mean_SR, median_SR,
                      mean_bf, median_bf)

# acceleration
ctc_ball_by_ball_mean <- career_mean_bbb(ctc)
model <- lm(ctc_ball_by_ball_mean$`mean SR` ~ ctc_ball_by_ball_mean$ball)
model$coefficients[2]

#---- function to produce metrics by tournament ----
# takes ball by ball data (for a tournament)
# innings_by_tournament <- split(conway_bbb_tibble, conway_bbb_tibble$tournament) need to split ball_by_ball_data by tournament before using with lapply
metrics_by_tournament <- function(x){
  x_innings_table <- innings_table(x)
  
  innings_n <- nrow(x)
  boundary_rate <- balls_per_boundary(x)
  dbp <- dot_ball_percentage(x)
  mean_runs_scored <- round(mean(x_innings_table$`total scored`))
  median_runs_scored <- round(median(x_innings_table$`total scored`))
  mean_SR <- round(mean(x_innings_table$SR))
  median_SR <- round(median(x_innings_table$SR))
  mean_balls_faced <- round(mean(x_innings_table$`balls faced`))
  median_balls_faced <- round(median(x_innings_table$`balls faced`))
  basra <- basra <- mean_runs_scored + mean_SR
  
  tournament_ball_by_ball_mean <- career_mean_bbb(x)
  model <- lm(tournament_ball_by_ball_mean$`mean SR` ~ tournament_ball_by_ball_mean$ball)
  
  tournament_summary_table <- tibble(innings_n, boundary_rate, dbp,
                                     mean_runs_scored, median_runs_scored,
                                     mean_SR, median_SR,
                                     mean_balls_faced, median_balls_faced,
                                     unname(model$coefficients[2]), basra) |>
    `colnames<-`(c("Innings",
                   "Balls per boundary",
                   "Dot ball %",
                   "Mean runs scored",
                   "Median runs scored",
                   "Mean strike rate",
                   "Median strike rate",
                   "Mean balls faced",
                   "Median balls faced",
                   "Acceleration",
                   "BASRA"))
  
}

conway_tournament_stats <- lapply(innings_by_tournament, metrics_by_tournament)
table1 <- conway_tournament_stats[[1]] |>
  mutate(tournament = names(conway_tournament_stats[1]),
         .before = 1)

y <- tibble()
for (i in 1:length(conway_tournament_stats)){
  x <- conway_tournament_stats[[i]] |>
    mutate(tournament = names(conway_tournament_stats[i]),
           .before = 1)
  
  y <- rbind(y, x)
}


#---- spider plot by tournament ----
par(mfrow = c(ceiling(length(conway_tournament_stats)/3),3))
for (i in 1:length(conway_tournament_stats)) {
  spider_plot(conway_tournament_stats[[i]], conway_innings_table, title = names(conway_tournament_stats[i]))
}


#===== by phase =====
#---- ball by ball by phase tibble ----
# add 1-6 powerplay, 7-15 middle, 16-20 death into a new column called phase in each tibble
# unite() to join over and ball columns
# what does the df need to look like? and how to join?
# each column is a ball for a phase, each row is a phase, cells contain runs scored
# it could look like the conway_bbb_tibble (ball_by_ball_data) but instead of tournament, it is phase so up to three entries can have the same match_id

b <- conway_innings_list[[1]] |> 
  mutate(phase = case_when(over < 6 ~ "powerplay",
                           over < 15 ~ "middle",
                           over < 21 ~ "death")) |>
  select(match_id, runs_off_bat, wides, phase) |>
  filter(is.na(wides)) |>
  select(!wides) |>
  group_by(phase) |>
  mutate(ball = row_number()) |>
  pivot_wider(names_from = ball, values_from = runs_off_bat)

#---- function to produce ball by ball by phase tibble ----
condense_by_phase <- function(x){
  y <- x |>
    mutate(phase = case_when(over < 6 ~ "powerplay",
                             over < 15 ~ "middle",
                             over < 21 ~ "death")) |>
    select(match_id, runs_off_bat, wides, phase) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    group_by(phase) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}

phase_bbb <- function(innings_list){
  x <- lapply(innings_list, condense_by_phase)
  y <- reduce(.x = x, .f = full_join)
  return(y)
}

conway_by_phase <- phase_bbb(conway_innings_list)


#---- calculate metrics by phase ----
# metrics by phase
metrics_by_phase <- conway_by_phase |>
  ungroup() |>
  group_by(phase) |>
  summarise(
    innings = n(),
    runs_scored = sum(across(3:ncol(conway_by_phase)-1), na.rm = T),
    mean_runs_scored = round(runs_scored/innings, 2),
    balls_faced = sum(!is.na(across(3:ncol(conway_by_phase)))),
    mean_SR = round(runs_scored/balls_faced*100, 2),
    BASRA = mean_runs_scored + mean_SR
  )
# count boundaries
fours <- tally(conway_by_phase, across(3:ncol(conway_by_phase)-1) == 4, name = "fours_count")
sixes <- tally(conway_by_phase, across(3:ncol(conway_by_phase)-1) == 6, name = "sixes_count")
# count dot balls
dot_balls <- tally(conway_by_phase, across(3:ncol(conway_by_phase)-1) == 0, name = "dot_ball_count")
# metrics by phase joined up
metrics_by_phase <- metrics_by_phase |>
  left_join(fours) |>
  left_join(sixes) |>
  left_join(dot_balls) |>
  mutate(boundary_rate = round(balls_faced/(fours_count + sixes_count), 2),
         dbp = round(dot_ball_count/balls_faced*100, 2))


#---- function to produce metrics by phase summary tibble ----
metrics_by_phase <- function(player_bbb_by_phase){
  # calculate runs, SR and BASRA
  x <- player_bbb_by_phase |>
    ungroup() |>
    group_by(phase) |>
    summarise(
      innings = n(),
      runs_scored = sum(across(3:ncol(player_bbb_by_phase)-1), na.rm = T),
      mean_runs_scored = round(runs_scored/innings, 2),
      balls_faced = sum(!is.na(across(3:ncol(player_bbb_by_phase)))),
      mean_balls_faced = balls_faced/innings,
      mean_SR = round(runs_scored/balls_faced*100, 2),
      BASRA = mean_runs_scored + mean_SR
    )
  
  # count boundaries
  fours <- tally(player_bbb_by_phase, across(3:ncol(player_bbb_by_phase)-1) == 4, name = "fours_count")
  sixes <- tally(player_bbb_by_phase, across(3:ncol(player_bbb_by_phase)-1) == 6, name = "sixes_count")
  
  # count dot balls
  dot_balls <- tally(player_bbb_by_phase, across(3:ncol(player_bbb_by_phase)-1) == 0, name = "dot_ball_count")
  
  # join metrics together
  y <- x |>
    left_join(fours, by = "phase") |>
    left_join(sixes, by = "phase") |>
    left_join(dot_balls, by = "phase") |>
    mutate(boundary_rate = round(balls_faced/(fours_count + sixes_count), 2),
           dbp = round(dot_ball_count/balls_faced*100, 2)) |>
    select(c("phase",
             "innings",
             "boundary_rate",
             "dbp",
             "mean_runs_scored",
             "mean_SR",
             "mean_balls_faced",
             "BASRA")) |>
    `colnames<-`(c("Phase",
                   "Innings",
                   "Balls per boundary",
                   "Dot ball %",
                   "Mean runs scored",
                   "Mean strike rate",
                   "Mean balls faced",
                   "BASRA"))
  
  return(y)
}

conway_metrics_by_phase_summary <- metrics_by_phase(conway_by_phase)

#---- spider plot by phase ----
# plotting balls per boundary, dot ball %, mean runs, mean SR, BASRA (no acceleration)
# what to use as max and min?
# max runs = max scored in a phase
# max SR = max SR in a phase
# BASRA = max runs + max SR
# max balls per boundary = 1, min = 20
# max dot ball % = 0, min = 100
phase_summary <- conway_by_phase |>
  ungroup() |>
  reframe(
    match_id = match_id,
    phase = phase,
    runs = rowSums(pick(where(is.numeric), -match_id), na.rm = T),
    balls_faced = rowSums(!is.na(pick(where(is.numeric), -match_id)))-1,
    SR = round(runs/balls_faced*100, 2),
    BASRA = round(runs + SR, 2)
  )

max_runs <- max(phase_summary$runs)
max_SR <- max(phase_summary$SR)
max_BASRA <- max(phase_summary$BASRA)

max_values <- c(1, 0, max_runs, max_SR, max_BASRA)
min_values <- c(20, 100, 0, 0, 0)

d <- conway_metrics_by_phase_summary |>
  select(c("Phase", "Balls per boundary", "Dot ball %", "Mean runs scored", "Mean strike rate", "BASRA"))

par(mar = c(2, 2, 2, 1), bg = "#FBFFF1", mfrow = c(1,3))
for (i in d[["Phase"]]) {
  e <- d |>
    filter(Phase == i) |>
    select(!Phase)
  
  e <- rbind(max_values, min_values, e)
  
  radarchart(df = e, axistype = 2,
             seg = 5, cglty = 2,
             cglcol = "lightgrey",
             axislabcol = "#6A8E7F",
             pfcol = alpha("#6A8E7F", 0.5),
             pcol = "#6A8E7F",
             vlabels = c("Balls per boundary", "Dot ball %", "Mean runs scored", "Mean strike rate", "BASRA"),
             vlcex = 1, title = i)
}

#---- function to produce spider plot by phase ----
spider_plot_by_phase <- function(player_bbb_by_phase, metrics_by_phase_summary){
  phase_summary <- player_bbb_by_phase |>
    ungroup() |>
    reframe(
      match_id = match_id,
      phase = phase,
      runs = rowSums(pick(where(is.numeric), -match_id), na.rm = T),
      balls_faced = rowSums(!is.na(pick(where(is.numeric), -match_id)))-1,
      SR = round(runs/balls_faced*100, 2),
      BASRA = round(runs + SR, 2)
    )
  
  max_runs <- max(phase_summary$runs)
  max_SR <- max(phase_summary$SR)
  max_BASRA <- max(phase_summary$BASRA)
  
  max_values <- c(1, 0, max_runs, max_SR, max_BASRA)
  min_values <- c(20, 100, 0, 0, 0)
  
  d <- metrics_by_phase_summary |>
    select(c("Phase", "Balls per boundary", "Dot ball %", "Mean runs scored", "Mean strike rate", "BASRA"))
  
  par(mar = c(1, 2, 2, 1), bg = "#FBFFF1", mfrow = c(1,3))
  
  for (i in d[["Phase"]]) {
    e <- d |>
      filter(Phase == i) |>
      select(!Phase)
    
    e <- rbind(max_values, min_values, e)
    
    colour <- if(i == "powerplay"){
      "#1A281F"
    } else if (i == "middle"){
      "#FFA630"
    } else if (i == "death") {
      "#776472"
    }
    
    radarchart(df = e, axistype = 2,
               seg = 5, cglty = 2,
               cglcol = "lightgrey",
               axislabcol = colour,
               pfcol = alpha(colour, 0.5),
               pcol = colour,
               vlabels = c("Balls per boundary", "Dot ball %", "Mean runs scored", "Mean strike rate", "BASRA"),
               vlcex = 1.2, title = i, cex.main = 2)
  }
}

spider_plot_by_phase(conway_by_phase, conway_metrics_by_phase_summary)
