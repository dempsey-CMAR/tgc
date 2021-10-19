#' Filter out observations that occur during heat stress events
#'
#' @details Filters out observations that occur during heat stress events, as
#'   defined by \code{identify_heat_stress_events()}.
#'
#'   Every observation that exceeds the threshold has a corresponding heat
#'   stress interval (\code{TIMESTAMP} of the observation + n_hours). Intervals
#'   may overlap with one or more other intervals.
#'
#'   Heat stress events are denoted by the beginning and end of overlapping
#'   intervals for each depth. Heat stress events do not overlap, but they may
#'   end and start on consecutive timestamps.
#'
#' @inheritParams identify_heat_stress_intervals
#'
#' @return Returns \code{dat}, filtered to remove observations that occur during
#'   heat stress events.
#'
#' @importFrom dplyr filter select
#' @importFrom lubridate as_datetime
#' @importFrom data.table setDT %inrange%
#' @importFrom purrr map_df
#'
#' @family heat stress
#'
#' @export


filter_out_heat_stress_events <- function(dat,
                                          heat_threshold = 18,
                                          n_hours = 24){

  # check how many different STATIONS are included in dat
  if("STATION" %in% colnames(dat)){

    n_stations <- length(unique(dat$STATION))

  } else n_stations <- 1

  # if only one station, use single filter function, otherwise use loop function
  if(n_stations == 1){

    filter_out_heat_stress_events_single(
      dat,
      heat_threshold = heat_threshold,
      n_hours = n_hours
    )

  } else {

    filter_out_heat_stress_events_loop(
      dat,
      heat_threshold = heat_threshold,
      n_hours = n_hours
    )
  }

}
