# Read data ----
nwhl_2021_data <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv')
oly_2018_data <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv')
oly_2022_data <- read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv')

# Process 2021 NWHL data ----
nwhl_2021_data_clean <- nwhl_2021_data %>%
  dplyr::mutate(
    Competition = "2021 NWHL"
  )

# Process 2018 Olympics data ----
oly_2018_data_clean <- oly_2018_data %>%
  dplyr::mutate(
    Competition = "2018 Olympics"
  )

# Process 2022 Olympics data ----
oly_2022_data_clean <- oly_2022_data %>%
  dplyr::rename(
    X.Coordinate = x_coord,
    Y.Coordinate = y_coord,
    X.Coordinate.2 = x_coord_2,
    Y.Coordinate.2 = y_coord_2,
    Event = event,
    Player = player_name,
    Period = period,
    Team = team_name,
  ) %>%
  dplyr::mutate(
    Detail.1 = ifelse(event_successful == "t", "Recovered", "Lost"),
    Home.Team = ifelse(venue == "home", Team, opp_team_name),
    Away.Team = ifelse(venue == "home", opp_team_name, Team),
    Home.Team.Goals = ifelse(venue == "home", goals_for, goals_against),
    Away.Team.Goals = ifelse(venue == "home", goals_against, goals_for),
    Competition = "2022 Olympics"
  ) %>%
  dplyr::select(
    game_date,
    Home.Team,
    Away.Team,
    Period,
    clock_seconds,
    Home.Team.Goals,
    Away.Team.Goals,
    Team,
    Player,
    Event,
    X.Coordinate,
    Y.Coordinate,
    Detail.1,
    X.Coordinate.2,
    Y.Coordinate.2,
    Competition
  )

# Combine dataframes ----
event_data <- plyr::rbind.fill(
  nwhl_2021_data_clean,
  oly_2018_data_clean,
  oly_2022_data_clean
  )

# TODO ----
# convert clock to clock_Seconds format for NWHL and 2018?
# oly_2022: convert situation_type to Home.Team.Skaters and Away.Team.Skaters

# Save data ----
usethis::use_data(event_data, overwrite = TRUE)
