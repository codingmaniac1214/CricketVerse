#---- summary stats plots ----
library(ggplot2)
library(fmsb)
library(dplyr)
library(tidyr)

#---- dummy data ----
innings <- 50
boundary_rate <- 5.2
dbp <- 19.6
mean_runs_scored <- 26.9
median_runs_scored <- 17.0
mean_SR <- 150.4
median_SR <- 132.1
mean_balls_faced <- 14
median_balls_faced <- 12
acceleration <- 0.174
BASRA <- 26.9 + 150.4

summary_table <- cbind(innings, boundary_rate, dbp,
                       mean_runs_scored, median_runs_scored,
                       mean_SR, median_SR,
                       mean_balls_faced, median_balls_faced,
                       acceleration, BASRA)


# only 6 metrics: boundary rate, dbp, runs, SR, acceleration, BASRA
# have to impose min and max values
# boundary rate = 20
# dbp = 100%
# runs = player's highest score
# SR = player's highest SR in an innings
# BASRA = player's highest BASRA
# acceleration = 4

data <- summary_table |>
  as_tibble() |>
  select(c("boundary_rate", "dbp", "median_runs_scored", "median_SR", "acceleration", "BASRA"))
max <- c(1, 0, 106, 240, 4, 55+180)
min <- c(20, 100, 0, 0, 0, 0)
data <- rbind(max, min, data)


#---- spider plot ----
radarchart(df = data, axistype = 2, 
           seg = 5, cglty = 2, 
           cglcol = "lightgrey", axislabcol = "#6A8E7F", 
           pfcol = alpha("#6A8E7F", 0.5), pcol = "#6A8E7F",
           vlabels = c("Balls per boundary", "Dot ball %", "Median runs scored", "Median SR", "Acceleration", "BASRA"), vlcex = 0.8)


#---- spider plot function ----
# want to take player data and then output the plot
spider_plot <- function(player_summary_table, player_innings) {
  player_summary_table <- player_summary_table |>
    as_tibble() |>
    select(c("Balls per boundary", "Dot ball %", "Median runs scored", "Median strike rate", "Acceleration", "BASRA"))
  
  max_runs <- max(player_innings$`total scored`)
  max_SR <- max(player_innings$SR)
  max_BASRA <- player_innings |>
    mutate(BASRA = `total scored` + SR) |>
    pull(BASRA) |>
    max()
  
  max_values <- c(1, 0, max_runs, max_SR, 4, max_BASRA)
  min_values <- c(20, 100, 0, 0, 0, 0)
  data <- rbind(max_values, min_values, player_summary_table)
  
  radarchart(df = data, axistype = 2,
             seg = 5, cglty = 2,
             cglcol = "lightgrey",
             axislabcol = "#6A8E7F",
             pfcol = alpha("#6A8E7F", 0.5),
             pcol = "#6A8E7F",
             vlabels = c("Balls per boundary", "Dot ball %", "Median runs scored", "Median SR", "Acceleration", "BASRA"), 
             vlcex = 0.8)
}


spider_plot(summary_table, innings_huss) # using innings_huss from experiments


#---- lollipop plot ----
columns <- c("max", "min", "player")
data_longer <- cbind(data, columns) |>
  pivot_longer(cols = 1:6, values_to = "value", names_to = "metric")

ggplot(data_longer) +
  geom_point(aes(x = value, y = metric))
# won't work because the metrics have very different scales