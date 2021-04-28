#' Filters out observations that occur during heat stress events
#'
#' @details Every observation that exceeds the threshold has a corresponding
#'   heat stress interval (\code{TIMESTAMP} of the observation + n_hours).
#'   Intervals may overlap with one or more other intervals.
#'
#'   Heat stress events are denoted by the beginning and end of overlapping
#'   intervals for each depth. Heat stress events do not overlap, but they may
#'   end and start on consecutive timestamps.
#'
#'
#' @inheritParams identify_heat_stress_intervals
#'
#' @return Returns dat, filtered to remove observations that occur during
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


filter_heat_stress_events <- function(dat,
                                      threshold = 18,
                                      n_hours = 24){

  dat <- dat %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP))

  heat_stress_events <- identify_heat_stress_events(dat = dat,
                                                    threshold = threshold,
                                                    n_hours = n_hours)

  depths <- unique(dat$DEPTH)

  # store filtered data in a list to speed up loop
  dat_filtered <- list()

  # loop over each depth and remove observations included in heat_stress_events
  for(i in seq_along(depths)){

    depth.i <- depths[i]

    dat.i <- filter(dat, DEPTH == depth.i)

    heat_event.i <- heat_stress_events %>%
      filter(DEPTH == depth.i) %>%
      select(-DEPTH, -event_id)

    dat_filtered[[i]] <- setDT(dat.i)[!(TIMESTAMP %inrange% heat_event.i)]

  }

  dat_filtered %>% map_df(rbind)

}
