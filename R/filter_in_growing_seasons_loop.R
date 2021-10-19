#' Keep observations that occur during growing seasons (for multiple stations)
#'
#' @details Keeps observations that occur during growing seasons as defined by
#'   \code{identify_growing_seasons()} (i.e., filters out observations that do
#'   not occur during the growing seasons).
#'
#'   This function works for one or more \code{STATION}s.
#'
#'   The growing season starts when the temperature crosses
#'   \code{trend_threshold} and does not return below \code{trend_threshold}
#'   (e.g., 4-degrees trending up) and ends 1 minute before the first
#'   observation of \code{superchill_threshold}.
#'
#'   The function looks for \code{START_SEASON} in January to August to avoid
#'   temperature drops caused by hurricanes (typically in September and October)
#'   and Winter cooling (November, December).
#'
#'   The function looks for \code{END_SEASON} in January to May of the next year
#'   (spring superchill).
#'
#' @inheritParams identify_growing_seasons
#'
#' @return Returns \code{dat}, filtered for observations that occur during
#'   growing seasons identified in \code{identify_growing_seasons}, with an
#'   additional column \code{SEASON} to label the seasons ("S1", "S2", ...).
#'   Some observations may be duplicated in consecutive seasons.

filter_in_growing_seasons_loop <- function(dat,
                                      trend_threshold = 4,
                                      superchill_threshold = -0.7,
                                      max_season = 540,
                                      full_season = TRUE){

  stations <- unique(dat$STATION)

  # store filtered data in a list to speed up loop
  st_dat_filtered <- list()

  # loop over each STATION and label seasons
  for(i in seq_along(stations)){

    station.i <- stations[i]

    dat.i <- filter(dat, STATION == station.i)

    st_dat_filtered[[i]] <- filter_in_growing_seasons_single(
      dat = dat.i,
      trend_threshold = trend_threshold,
      superchill_threshold = superchill_threshold,
      max_season = max_season,
      full_season = full_season
    )
  }

  st_dat_filtered %>% map_df(rbind)

}
