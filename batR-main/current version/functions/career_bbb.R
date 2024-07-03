#---- function to filter out wides and put each ball in a column----
condense_career <- function(x){
  y <- x |>
    select(match_id, runs_off_bat, wides, tournament) |>
    filter(is.na(wides)) |>
    select(!wides) |>
    mutate(ball = row_number()) |>
    pivot_wider(names_from = ball, values_from = runs_off_bat)
  return(y)
}


#---- apply condense_career to items in a list and join the resulting tibbles to create ball by ball data ----
career_bbb <- function(innings_list){
  x <- lapply(innings_list, condense_career)
  y <- reduce(.x = x, .f = full_join)
  return(y)
}
