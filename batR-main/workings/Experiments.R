#---- Experiments ----
library(cricketdata)
library(tidyverse)


#---- Who is the most consistent batsman in T20 internationals? ----
tournaments <- cricsheet_codes

# fetch innings by innings data
# t20_innings_data <- fetch_cricinfo(matchtype = "t20",
#                                   activity = "batting",
#                                   sex = "men", 
#                                   type = "innings")
t20_innings_data <- read_rds("t20_innings_data")


# filter our those who have not played more than 30 innings
unique(t20_innings_data$Participation)

min_30_innings <- t20_innings_data |>
  filter(Participation == "B") |>
  count(Player, name = "Innings") |>
  filter(Innings >= 30)

t20_innings_filtered <- t20_innings_data |>
  filter(Player %in% min_30_innings$Player)


# calculate summary stats per batsman
t20_stats <- t20_innings_filtered |>
  group_by(Player) |>
  drop_na(Runs, StrikeRate) |>
  summarise(
    # runs
    total_runs = sum(Runs),
    mean_score = mean(Runs), # mean score = average per innings (doesn't matter if the player is not out)
    median_score = median(Runs),
    sd_score = sd(Runs),
    
    # SR
    median_SR = median(StrikeRate),
    sd_SR = sd(StrikeRate),
    
    # balls faced
    mean_balls_faced = mean(BallsFaced),
    median_balls_faced = median(BallsFaced),
    sd_balls_faced = sd(BallsFaced),
    total_balls_faced = sum(BallsFaced),
    
    # boundaries
    total_boundaries = sum(Fours) + sum(Sixes)
  ) |>
  mutate(balls_per_boundary = total_balls_faced/total_boundaries,
         mean_SR = mean_score/mean_balls_faced*100) |>
  mutate(across(where(is.numeric), ~round(.x, 2)))


# plot strike rates
ggplot(t20_stats |>
         filter(mean_score > 26)) +
  geom_point(aes(mean_SR, Player), colour = "firebrick") +
  geom_point(aes(median_SR, Player), colour = "steelblue") +
  geom_vline(xintercept = mean(t20_stats$mean_SR), colour = "red", alpha = 0.7) +
  geom_vline(xintercept = mean(t20_stats$median_SR), colour = "blue", alpha = 0.7) +
  theme_classic() +
  scale_x_continuous(n.breaks = 10)

ggplot(t20_stats |>
         mutate(diff_sr = median_SR - mean_SR) |>
         filter(mean_score > 26)) +
  geom_col(aes(diff_sr, Player)) +
  theme_classic()


# plot scores
ggplot(t20_stats |>
         filter(mean_score > 26)) +
  geom_point(aes(mean_score, Player), colour = "firebrick") +
  geom_point(aes(median_score, Player), colour = "steelblue") +
  theme_classic() +
  scale_x_continuous(n.breaks = 10)

ggplot(t20_stats |>
         mutate(diff_score = median_score - mean_score) |>
         filter(mean_score > 26)) +
  geom_col(aes(diff_score, Player)) +
  theme_classic()


# plot balls faced
ggplot(t20_stats |>
         filter(mean_balls_faced > 20)) +
  geom_point(aes(mean_balls_faced, Player), colour = "firebrick") +
  geom_point(aes(median_balls_faced, Player), colour = "steelblue") +
  theme_classic() +
  scale_x_continuous(n.breaks = 10)


# plot balls per boundary
ggplot(t20_stats |>
         filter(total_boundaries > 190)) +
  geom_point(aes(balls_per_boundary, Player)) +
  geom_vline(xintercept = 6) +
  theme_classic() +
  scale_x_continuous(n.breaks = 10)



#---- calculate ball by ball metrics for Mike Hussey in T20 cricket ----
### part 1 - obtain data from every league
# obtain data for Mike Hussey from every league
# Huss played T20I, IPL, CPL, BBL
# huss_t20i <- fetch_cricsheet(type = "bbb",
#                              gender = "male",
#                              competition = "t20s") |>
#   filter(striker == "MEK Hussey") |>
#   mutate(tournament = paste0("international"))
# 
# huss_ipl <- fetch_cricsheet(type = "bbb",
#                             gender = "male",
#                             competition = "ipl") |>
#   filter(striker == "MEK Hussey")
# 
# huss_cpl <- fetch_cricsheet(type = "bbb",
#                             gender = "male",
#                             competition = "cpl") |>
#   filter(striker == "MEK Hussey")
# 
# huss_bbl <- fetch_cricsheet(type = "bbb",
#                             gender = "male",
#                             competition = "bbl") |>
#   filter(striker == "MEK Hussey")

# loop through leagues to obtain data
# innings_list <- list()
# for (i in mens_t20_tournaments){
#   x <- fetch_cricsheet(type = "bbb",
#                        gender = "male",
#                        competition = i) |>
#     filter(striker == player_name)
#   
#   innings_list[[i]] <- x
# }


### part 2 - split the data into a list where each innings is an item
# huss_t20i_split <- split(huss_t20i, f = huss_t20i$match_id)


### part 3 - create a data frame where each row is an innings and each column is the number ball faced (ball 1, ball 2, etc)
# a <- huss_t20i_split$"211028"
# b <- a |>
#   select(match_id, runs_off_bat, wides) |>
#   filter(is.na(wides)) |>
#   select(!wides) |>
#   mutate(ball = paste("ball", row_number())) |>
#   pivot_wider(names_from = ball, values_from = runs_off_bat)
# huss_t20i_split$"211028" <- b
# perform the above on the whole list by creating a function to execute the wrangling then call the function with lapply to iterate over the whole list

# huss_bbb <- career_bbb(huss_t20i_split) # for just t20i data - before career_bbb and condense were modified


### WORKFLOW ###
# vector of tournaments
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma")) |>
  pull(code)

# does parts 1 and 2
# function that takes a player's name, loops through the vector of tournaments and obtains ball by ball data for a player.
# then splits the data into a list where each innings is an item
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
bbb_by_innings <- find_bbb_male("MEK Hussey")

# does part 3
# function to filter out wides and place the balls as columns
condense <- function(x){
  y <- x |>
    select(match_id, runs_off_bat, wides, tournament) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}
# apply condense to items in a list and join the results
career_bbb <- function(innings_list){
  x <- lapply(innings_list, condense)
  y <- reduce(.x = x, .f = full_join)
  return(y)
}

huss <- career_bbb(bbb_by_innings) # ball by ball data for all t20 innings


# over Hussey's career, calculate mean runs scored and mean SR by ball
# overall_bbb_mean_huss <- huss |>
  # select(!match_id & !tournament) |>
  # summarise(across(where(is.numeric), mean, na.rm = T)) |>
  # mutate(across(where(is.numeric), ~round(.x, 2))) |>
  # pivot_longer(cols = everything(), names_to = "ball", values_to = "mean scored") |>
  # mutate(ball = as.numeric(ball),
  #        "mean SR" = `mean scored`*100)
career_mean_bbb <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    select(!match_id & !tournament) |>
    summarise(across(where(is.numeric), mean, na.rm = T)) |>
    mutate(across(where(is.numeric), ~round(.x, 2))) |>
    pivot_longer(cols = everything(), names_to = "ball", values_to = "mean scored") |>
    mutate(ball = as.numeric(ball),
           "mean SR" = `mean scored`*100)
  
  return(x)
}
overall_bbb_mean_huss <- career_mean_bbb(huss)
DT::datatable(overall_bbb_mean_huss, rownames = F)

# cor.test(overall_bbb_mean_huss$`mean SR`, overall_bbb_mean_huss$ball)
model <- lm(overall_bbb_mean_huss$`mean SR` ~ overall_bbb_mean_huss$ball)
# summary(model)

ggplot(overall_bbb_mean_huss) +
  geom_point(aes(ball, `mean SR`)) +
  geom_hline(yintercept = mean_SR, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(n.breaks = nrow(overall_bbb_mean_huss)/5) +
  geom_abline(slope = model$coefficients[2], intercept = model$coefficients[1], alpha = 0.5) +
  theme(plot.background = element_rect(fill = "#FBFFF1"),
        panel.background = element_rect(fill = "#FBFFF1"),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))


# calculate the mean runs scored per ball over Hussey's career by tournament
# tournament_bbb_mean_huss <- huss |>
#   group_by(tournament) |>
#   select(!match_id) |>
#   summarise(across(where(is.numeric), mean, na.rm = T)) |>
#   mutate(across(where(is.numeric), ~round(.x, 2))) |>
#   pivot_longer(cols = 2:(ncol(huss)-1), names_to = "ball", values_to = "mean scored") |>
#   mutate(ball = as.numeric(ball),
#          "mean SR" = `mean scored`*100)
tournament_mean_bbb <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    group_by(tournament) |>
    select(!match_id) |>
    summarise(across(where(is.numeric), mean, na.rm = T)) |>
    mutate(across(where(is.numeric), ~round(.x, 2))) |>
    pivot_longer(cols = 2:(ncol(huss)-1), names_to = "ball", values_to = "mean scored") |>
    mutate(ball = as.numeric(ball),
           "mean SR" = `mean scored`*100)
  
  return(x)
}
tournament_bbb_mean_huss <- tournament_mean_bbb(huss)

ggplot(tournament_bbb_mean_huss) +
  geom_point(aes(ball, `mean SR`, colour = tournament), size = 0.7) +
  geom_smooth(aes(ball, `mean SR`, colour = tournament), method = "lm", se = F) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) + # could add career mean SR line
  theme_classic() +
  scale_x_continuous(n.breaks = nrow(overall_bbb_mean_huss)/5) +
  geom_abline(slope = model$coefficients[2], intercept = model$coefficients[1], alpha = 0.7)



#---- calculate career innings metrics for Mike Hussey in T20 cricket ----
# runs
# huss_runs <- huss |>
#   pivot_longer(cols = 3:ncol(huss), names_to = "ball", values_to = "runs per ball") |>
#   drop_na(`runs per ball`) |>
#   mutate(match_id = as_factor(match_id)) |>
#   group_by(match_id) |>
#   summarise("total scored" = sum(`runs per ball`))
# huss_median_score <- round(median(huss_runs$`total scored`), 2)
# huss_mean_score <- round(mean(huss_runs$`total scored`), 2)

# SR and balls faced
# huss_SR <- huss |>
#   pivot_longer(cols = 3:ncol(huss), names_to = "ball", values_to = "runs scored") |>
#   drop_na(`runs scored`) |>
#   count(as_factor(match_id)) |>
#   rename(match_id = `as_factor(match_id)`,
#          "balls faced" = n) |>
#   left_join(huss_runs) |>
#   mutate(SR = round(`total scored`/`balls faced`*100, 2))
innings_table <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    pivot_longer(cols = 3:ncol(ball_by_ball_data), names_to = "ball", values_to = "runs per ball") |>
    drop_na(`runs per ball`) |>
    mutate(match_id = as_factor(match_id)) |>
    group_by(match_id) |>
    summarise("total scored" = round(sum(`runs per ball`), 2))
  
  y <- ball_by_ball_data |>
    pivot_longer(cols = 3:ncol(ball_by_ball_data), names_to = "ball", values_to = "runs scored") |>
    drop_na(`runs scored`) |>
    count(as_factor(match_id)) |>
    rename(match_id = `as_factor(match_id)`,
           "balls faced" = n) |>
    left_join(x) |>
    mutate(SR = round(`total scored`/`balls faced`*100, 2))
  
  return(y)
}
innings_huss <- innings_table(huss)

mean_runs_scored <- round(mean(innings_huss$`total scored`), 2)
median_runs_scored <- median(innings_huss$`total scored`)
mean_SR <- round(sum(innings_huss$`total scored`)/sum(innings_huss$`balls faced`)*100, 2)
median_SR <- median(innings_huss$SR)
mean_balls_faced <- round(mean(innings_huss$`balls faced`), 2)
median_balls_faced <- median(innings_huss$`balls faced`)


# balls per boundary
balls_per_boundary <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  balls_faced <- sum(balls, na.rm = T)
  boundary_count <- sum(balls == 4 | balls == 6, na.rm = T)
  balls_per_boundary_rate <- round(balls_faced/boundary_count, 2)
  return(balls_per_boundary_rate)
}
balls_per_boundary_rate <- balls_per_boundary(huss)

# dot ball %
dot_ball_percentage <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  dot_ball_count <- sum(balls == 0, na.rm = T)
  balls_faced <- sum(balls, na.rm = T)
  dot_ball_percent <- round(dot_ball_count/balls_faced*100, 2)
  return(dot_ball_percent)
}
dot_ball_p <- dot_ball_percentage(huss)


# create table
summary_stats <- cbind(balls_per_boundary_rate, dot_ball_p, mean_runs_scored, median_runs_scored, mean_SR, median_SR, mean_balls_faced, median_balls_faced) |>
  `colnames<-`(c("Ball per boundary",
                 "Dot ball %",
                 "Mean runs scored",
                 "Median runs scored",
                 "Mean strike rate",
                 "Median strike rate",
                 "Mean balls faced",
                 "Median balls faced"))


(summary_stats[5]-model$coefficients[1])/model$coefficients[2]

balls_to_reach_mean_SR <- function(mean_SR, model){
  (mean_SR-model$coefficients[1])/model$coefficients[2]
}