#---- phase_bbb ----
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
