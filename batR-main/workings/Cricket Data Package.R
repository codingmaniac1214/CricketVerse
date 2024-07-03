#---- Cricket Data Package ----
library(cricketdata)


#---- find player ID from cricinfo ----
# most functions use the player's cricinfo ID to fetch their career data
a <- find_player_id("Perry")
# returns a tibble with all the IDs of players who have Perry in their name


#---- fetch data from cricinfo ----
# international matches only
# type = "innings" or "career"

b <- fetch_cricinfo(matchtype = "test",
                    sex = "women",
                    activity = "batting",
                    type = "career")
# tibble of player career stats

c <- fetch_cricinfo(matchtype = "t20",
                    sex = "men",
                    activity = "bowling",
                    type = "innings",
                    country = "Ireland")
# tibble of innings by innings bowling/batting record for every player
# note: participation indicates if the player bowled or batted



#---- fetch player data from cricinfo ----
d <- fetch_player_data(playerid = 275487,
                       matchtype = "odi",
                       activity = "batting")
# tibble of player statistics by innings



#---- fetch ball by ball, match and player data from cricsheet ----
tournaments <- cricsheet_codes # full list of tournaments for the competition parameter

# type = "bbb", "match", "player"
e <- fetch_cricsheet(type = "bbb", 
                     gender = "male", 
                     competition = "sat")
# a tibble describing what happened every ball for every match in a tournament's history

g <- fetch_cricsheet(type = "player",
                     gender = "male",
                     competition = "sat")
# a tibble of match IDs with the teams that played and the XIs fielded

h <- fetch_cricsheet(type = "match",
                     gender = "male",
                     competition = "sat")
# tibble giving an overview of certain match details



#---- fetch non-match data about the player (including batting style and bowling style) ----
f <- fetch_player_meta(playerid = 275487)
# tibble of the player's non-match data


