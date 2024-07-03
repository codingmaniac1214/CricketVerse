#---- metrics_by_tournament ----
# takes ball by ball data for a tournament
# call split(ball_by_ball_data, ball_by_ball_data$tournament) to create ball by ball data for a tournament
# then use lapply(output of the above, metrics_by_tournament)
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
