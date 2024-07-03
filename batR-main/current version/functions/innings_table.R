#---- create a tibble of all the innings a player has played ----
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
