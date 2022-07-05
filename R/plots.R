#' Plot Players' RIPP per Game vs Goals per Game
#'
#' Build plot of players' average RIPP per game vs their goals per game
#'
#' @param event_values A data frame of event data
#' @return A formatted ggplot
#' @export
player_ripp_vs_goals <- function(event_values) {
  player_values <- event_values %>%
    dplyr::group_by(Player) %>%
    dplyr::summarize(
      RIPP = sum(RIPP),
      Matches = length(unique(game_date)),
      RIPP_per_game = RIPP / Matches,
      goals_per_game = sum(Event == "Goal")/ Matches,
      n_plays = dplyr::n(),
      team = dplyr::first(Team),
      Competition = dplyr::first(Competition)
    )

  player_values %>%
    ggplot2::ggplot(ggplot2::aes(x = RIPP_per_game, y = goals_per_game)) +
    ggplot2::geom_point(ggplot2::aes(cex = Matches, col = Competition), alpha = 0.5) +
    ggplot2::geom_smooth(method = lm, color = "blue", se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(x = "RIPP per game", y = "Goals per game")
}


#' Plot Players' RIPP per Game vs Goals per Game
#'
#' Build bar plot of player's RIPP per game per event type vs the average player
#'
#' @param event_values A data frame of event data
#' @param our_player The name of the player of interest
#' @return A formatted ggplot
#' @export
player_vs_average <- function(event_values, our_player) {
  our_player_values <- event_values %>%
    dplyr::filter(Player == our_player) %>%
    dplyr::mutate(Matches = length(unique(game_date))) %>%
    dplyr::group_by(Event) %>%
    dplyr::summarise(RIPP_avg = sum(RIPP) / dplyr::first(Matches)) %>%
    tidyr::complete(Event) %>%
    dplyr::mutate(
      RIPP_avg = tidyr::replace_na(RIPP_avg, 0)
    ) %>%
    dplyr::mutate(Player = our_player)

  average_player_values <- event_values %>%
    # Step 1: Add column with number of matches per player
    dplyr::mutate(
      Player = as.factor(Player),
      Event = as.factor(Event)
    ) %>%
    dplyr::group_by(Player) %>%
    dplyr::mutate(Matches = length(unique(game_date))) %>%
    dplyr::ungroup() %>%
    # Step 2: Compute average RIPP per game for each event type for each player
    dplyr::group_by(Player, Event) %>%
    dplyr::summarise(RIPP_avg = sum(RIPP) / dplyr::first(Matches)) %>%
    tidyr::complete(Player, Event) %>%
    dplyr::mutate(
      RIPP_avg = tidyr::replace_na(RIPP_avg, 0)
    ) %>%
    # Step 3: Compute RIPP per game metrics for average player
    dplyr::group_by(Event) %>%
    dplyr::summarise(RIPP_avg = mean(RIPP_avg)) %>%
    dplyr::mutate(Player = "Average")


  rbind(average_player_values, our_player_values) %>%
    ggplot2::ggplot(ggplot2::aes(x = Event, y = RIPP_avg, fill=Player)) +
    ggplot2::geom_bar(position="dodge", stat="identity") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "RIPP per game")
}


#' Plot Teams' RIPP per Game vs Goals per Game
#'
#' Build plot of teams' average RIPP per game vs their goals per game
#'
#' @param event_values A data frame of event data
#' @return A formatted ggplot
#' @export
team_ripp_vs_goals <- function(event_values) {
  team_values <- event_values %>%
    dplyr::group_by(Team) %>%
    dplyr::summarize(
      RIPP = sum(RIPP),
      Matches = length(unique(game_date)),
      RIPP_per_game = RIPP / Matches,
      goals_per_game = sum(Event == "Goal")/ Matches,
      n_plays = dplyr::n(),
      Competition = dplyr::first(Competition)
    )

  team_values %>%
    ggplot2::ggplot(ggplot2::aes(x = RIPP_per_game, y = goals_per_game)) +
    ggplot2::geom_point(ggplot2::aes(cex = Matches, col = Competition), alpha = 0.5) +
    ggplot2::geom_smooth(method = lm, color = "blue", se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(x = "RIPP per game", y = "Goals per game")
}
