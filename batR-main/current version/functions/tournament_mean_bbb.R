#---- calculate mean runs scored and mean SR by ball faced broken down by tournament ----
tournament_mean_bbb <- function(ball_by_ball_data){
  x <- ball_by_ball_data |>
    group_by(tournament) |>
    select(!match_id) |>
    summarise(across(where(is.numeric), mean, na.rm = T)) |>
    mutate(across(where(is.numeric), ~round(.x, 2))) |>
    pivot_longer(cols = 2:(ncol(ball_by_ball_data)-1), names_to = "ball", values_to = "mean scored") |>
    mutate(ball = as.numeric(ball),
           "mean SR" = `mean scored`*100)
  
  return(x)
}
