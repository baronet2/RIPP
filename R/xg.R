#' Compute Distance to Goal
#'
#' Compute the distances from points to the centre of the net
#'
#' @param x A numeric vector of the x coordinates
#' @param y A numeric vector of the y coordinates
#' @return A numeric vector of the computed distances
#' @export
distance_to_net <- function(x, y)
{
  sqrt(((189 - x)^2 + (42.5 - y)^2))
}

#' Compute Angle to Goal
#'
#' Compute the angles from points to the centre of the net
#'
#' @param x A numeric vector of the x coordinates
#' @param y A numeric vector of the y coordinates
#' @return A numeric vector of the computed angles
#' @export
angle_to_net <- function(x, y)
{
  atan(abs(42.5 - y)/abs(189 - x))
}

#' Prepare Expected Goals Features
#'
#' Add any features needed for the expected goals model
#'
#' @param data A data frame containing \code{X.Coordinate} and \code{Y.Coordinate} columns
#' @return The same data frame with additional columns for any expected goals features
#' @export
prepare_xg_features <- function(data)
{
  data %>%
    dplyr::mutate(
      Distance = distance_to_net(X.Coordinate, Y.Coordinate),
      Angle = angle_to_net(X.Coordinate, Y.Coordinate)
    )
}

#' Predict Expected Goals
#'
#' Predict the expected goals for shots with start coordinates (x, y)
#'
#' @param x A numeric vector of the x coordinates
#' @param y A numeric vector of the y coordinates
#' @return A numeric vector of the computed expected goals values
#' @export
predict_xg <- function(x, y)
{
  data <- data.frame(X.Coordinate = x,
                     Y.Coordinate = y) %>%
    prepare_xg_features()

  predict(xg_model, data, type="response")
}
