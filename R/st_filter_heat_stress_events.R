#' Filter out heat stress events (from multiple stations)
#'
#' @details Filters out observations that occur during heat stress events, as
#'   defined by \code{identify_heat_stress_events()}.
#'
#'   This function works for one or more \code{STATION}s.
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
#' @param dat Dataframe with at least four columns: \code{TIMESTAMP} (must be
#'   possible to convert to a Date object), \code{DEPTH}, \code{STATION}, and
#'   \code{VALUE}. If column \code{VARIABLE} is included, it must have one
#'   unique entry. Other columns will be ignored.
#'
#' @return Returns dat, filtered to remove observations that occur during heat
#'   stress events.
#'
#' @importFrom dplyr filter
#' @importFrom purrr map_df
#'
#' @family heat stress
#'
#' @export


st_filter_heat_stress_events <- function(dat,
                                         heat_threshold = 18,
                                         n_hours = 24){

  stations <- unique(dat$STATION)

  # store filtered data in a list to speed up loop
  st_dat_filtered <- list()

  # loop over each STATION and remove observations included in heat_stress_events
  for(i in seq_along(stations)){

    station.i <- stations[i]

   # print("st_filter_heat_stress_events(): filtering station", station.i)

    dat.i <- filter(dat, STATION == station.i)

    st_dat_filtered[[i]] <- filter_heat_stress_events(
      dat = dat.i,
      heat_threshold = heat_threshold,
      n_hours = n_hours
    )
  }

  st_dat_filtered %>% map_df(rbind)

}
