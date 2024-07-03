#---- spider_plot_by_phase ----
# takes the output of phase_bbb and metrics_by_phase
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
  
  
  par(mar = c(3, 3, 2, 1), bg = "#FBFFF1", mfrow = c(1,3))
  
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