#' Apply filters to prepare data for degree-day calculation
#'
#' @inheritParams identify_heat_stress_intervals
#'
#' @inheritParams filter_in_growing_seasons
#'
#' @return Returns \code{dat} filtered for days that will be used to calculate
#'   degree-days at \code{DEPTH}. Includes observations that start after the
#'   4-degree trending up threshold and end 1 minute before the first
#'   observation of superchill for each season; observations that occur during a
#'   heat stress events are removed.
#'
#'   An additional column \code{SEASON} is included to label the growing seasons
#'   ("S1", "S2", ...). Some observations may be duplicated in consecutive
#'   seasons.
#'
#' @export


apply_dd_filters <- function(dat,
                             trend_threshold = 4,
                             superchill_threshold = -0.7,
                             max_season = 540,
                             full_season = TRUE,
                             heat_threshold = 18,
                             n_hours = 24){

  dat %>%
    filter_in_growing_seasons(
      trend_threshold = trend_threshold,
      superchill_threshold = superchill_threshold,
      max_season = max_season,
      full_season = full_season
    ) %>%
    filter_out_heat_stress_events(
      heat_threshold = heat_threshold,
      n_hours = n_hours
    )

}
