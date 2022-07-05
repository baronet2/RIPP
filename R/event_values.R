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
      start.xG = predict_xg(X.Coordinate, Y.Coordinate),
      end.xG = predict_xg(X.Coordinate.2, Y.Coordinate.2),
      nextEvent.xG = dplyr::lead(start.xG, 1),
      previousEvent.xG = dplyr::lag(start.xG, 1),

      # Reflect coordinates before computing xG
      teamB.faceoff.xG = predict_xg(200 - X.Coordinate, 85 - Y.Coordinate),
      faceoff.startValue = 0.5 * start.xG - 0.5 * teamB.faceoff.xG,
      shot.endValue = ifelse(dplyr::lead(Event, 1) == "Faceoff Win",
                             # Shot led to faceoff - who won the faceoff?
                             ifelse(dplyr::lead(Team, 1) == Team, faceoff.startValue, -faceoff.startValue),
                             # Shot led to rebound - who got the rebound?
                             ifelse(dplyr::lead(Team, 1) == Team, nextEvent.xG, -nextEvent.xG)),
      dump.endValue = ifelse(Detail.1 == "Recoverd", nextEvent.xG, -nextEvent.xG),

      # Event values
      pass.value = end.xG - start.xG,
      turnover.value = - nextEvent.xG -start.xG,
      recovery.value = start.xG + previousEvent.xG,
      faceoffWin.value = nextEvent.xG - faceoff.startValue,
      shot.value = shot.endValue - start.xG,
      goal.value = 1 - start.xG,
      dump.value = dump.endValue - start.xG,

      # RIPP
      RIPP = dplyr::case_when(
        Event == "Play" ~ pass.value,
        Event == "Incomplete Play" ~ turnover.value,
        Event == "Puck Recovery" ~ recovery.value,
        Event == "Dump In/Out" ~ dump.value,
        Event == "Faceoff Win" ~ faceoffWin.value,
        Event == "Goal" ~ goal.value,
        Event == "Shot" ~ shot.value,
        TRUE ~ NA_real_)
    ) %>%
    dplyr::pull(RIPP)
}

# TODO ----
# Separate this into a few functions
# Have one column with action start value and one column with action end value
