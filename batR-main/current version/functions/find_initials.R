#---- Find initials ----
find_initials <- function(player_to_find){
  d <- find_player_id(player_to_find) |>
    select(c("Name", "Country", "Played"))
}
