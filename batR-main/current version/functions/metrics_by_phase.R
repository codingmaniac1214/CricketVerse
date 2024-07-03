#---- metrics_by_phase ----
# takes the output of phase_bbb
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