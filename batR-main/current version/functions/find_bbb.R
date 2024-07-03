#---- get ball by ball data of a player. Generates a list where each item is an innings ----
find_bbb <- function(player_name, gender, mens_t20_data, womens_t20_data){
  if(gender == "male"){
    x <- mens_t20_data |>
      filter(striker == player_name)
  } else if(gender == "female"){
    x <- womens_t20_data |>
      filter(striker == player_name)
  }
  
  y <- split(x, f = x$match_id)
  
  return(y)
}
