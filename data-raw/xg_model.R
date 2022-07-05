library(RIPP)
data(event_data, package = "RIPP")

shots <- event_data %>%
  prepare_xg_features() %>%
  dplyr::filter(Event %in% c("Goal", "Shot")) %>%
  dplyr::mutate(Goal = (Event == "Goal"))

xg_model = glm(Goal ~ Distance + Angle, data = shots, family = "binomial")
usethis::use_data(xg_model, overwrite = TRUE, internal = TRUE)
