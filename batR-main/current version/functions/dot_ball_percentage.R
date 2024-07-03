#---- calculate dot ball % ----
dot_ball_percentage <- function(ball_by_ball_data){
  balls <- ball_by_ball_data[3:ncol(ball_by_ball_data)]
  dot_ball_count <- sum(balls == 0, na.rm = T)
  balls_faced <- sum(balls, na.rm = T)
  dot_ball_percent <- round(dot_ball_count/balls_faced*100, 2)
  return(dot_ball_percent)
}
