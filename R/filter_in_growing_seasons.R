#' Keep observations that occur during growing seasons
#'
#' @details Keeps observations that occur during growing seasons as defined by
#'   \code{identify_growing_seasons()} (i.e., filters out observations that do
#'   not occur during the growing seasons).
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
#' @param full_season Logical argument. The default, \code{full_season = TRUE}
#'   will only return data for groups with a full season of data (e.g.,
#'   \code{START_SEASON} and \code{END_SEASON} as returned by
#'   \code{identify_growing_seasons(dat, full_season = TRUE)} are both NOT
#'   \code{NA}).
#'
#'   If \code{full_season = FALSE}, data for all groups will be returned;
#'   the \code{START_SEASON} and \code{END_SEASON} dates default to the first
#'   and last timestamp in \code{...} and \code{DEPTH}.
#'
#' @return Returns \code{dat}, filtered to keep observations that occur during
#'   growing seasons identified in \code{identify_growing_seasons}, with an
#'   additional column \code{SEASON} to label the seasons ("S1", "S2", ...).
#'   Observations may be duplicated in consecutive seasons.
#'
#' @importFrom data.table setDT %inrange%
#' @importFrom purrr map_df
#'
#' @export

filter_in_growing_seasons <- function(dat,
                                      full_season = TRUE,
                                      trend_threshold = 4,
                                      superchill_threshold = -0.7,
                                      max_season = 540){

  # check how many different STATIONS are included in dat
  if("STATION" %in% colnames(dat)){

    n_stations <- length(unique(dat$STATION))

  } else n_stations <- 1

  # if only one station, use single filter function, otherwise use loop function
  if(n_stations == 1){

    dat %>%
      filter_in_growing_seasons_single(
        trend_threshold = trend_threshold,
        superchill_threshold = superchill_threshold,
        max_season = max_season,
        full_season = full_season
      )

  } else {

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

}
