#' Compute RIPP
#'
#' Compute RIPP given event data frame
#'
#' @param data A data frame of event data
#' @return A numeric vector of RIPP values
#' @export
compute_ripp <- function(data) {
  data %>%
    dplyr::mutate(
      start_xg = predict_xg(X.Coordinate, Y.Coordinate),
      end_xg = predict_xg(X.Coordinate.2, Y.Coordinate.2),
      next_event_xg = dplyr::lead(start_xg),
      previous_event_xg = dplyr::lag(start_xg),
      opp_xg = predict_xg(200 - X.Coordinate, 85 - Y.Coordinate), # Reflect coordinates

      faceoff_start_value = 0.5 * start_xg - 0.5 * opp_xg,
      shot_end_value = ifelse(dplyr::lead(Event) == "Faceoff Win",
                             # Shot led to faceoff - who won the faceoff?
                             ifelse(dplyr::lead(Team) == Team,
                                    dplyr::lead(faceoff_start_value),
                                    - dplyr::lead(faceoff_start_value)),
                             # Shot led to rebound - who got the rebound?
                             ifelse(dplyr::lead(Team) == Team, next_event_xg, -next_event_xg)),

      start_value = dplyr::recode(
        Event,
        "Play" = start_xg,
        "Incomplete Play" = start_xg,
        "Puck Recovery" = ifelse(Team == dplyr::lag(Team), previous_event_xg, - previous_event_xg),
        "Dump In/Out" = ifelse(Detail.1 == "Recovered", next_event_xg, -next_event_xg),
        "Faceoff Win" = 0.5 * start_xg - 0.5 * opp_xg,
        "Goal" = start_xg,
        "Shot" = start_xg,
        .default = NA_real_
      ),

      end_value = dplyr::recode(
        Event,
        "Play" = end_xg,
        "Incomplete Play" = - next_event_xg,
        "Puck Recovery" = start_xg,
        "Dump In/Out" = start_xg,
        "Faceoff Win" = next_event_xg,
        "Goal" = 1,
        "Shot" = shot_end_value,
        .default = NA_real_
      ),

      RIPP = end_value - start_value
    ) %>%
    dplyr::pull(RIPP)
}

# TODO ----
# Use state value model instead of xG model
# Include remaining event types
