#' Keep observations that occur during growing seasons
#'
#' @details Keeps observations that occur during growing seasons as defined by
#'   \code{identify_growing_seasons()} (i.e., filters out observations that do
#'   not occur during the growing seasons).
#'
#'   This function only works for a single \code{STATION}. For multiple
#'   \code{STATION}s, use \code{st_filter_growing_seasons()}.
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
#' @return Returns \code{dat}, filtered to keep observations that occur during
#'   growing seasons identified in \code{identify_growing_seasons}, with an
#'   additional column \code{SEASON} to label the seasons ("S1", "S2", ...).
#'   Observations may be duplicated in consecutive seasons.
#'
#' @export

filter_growing_seasons <- function(dat,
                                   trend_threshold = 4,
                                   superchill_threshold = -0.7,
                                   max_season = 18){

  # check how many different STATIONS are included in dat
  if("STATION" %in% colnames(dat)){

    n_stations <- length(unique(dat$STATION))

    if(n_stations > 1){

      stop("More than one STATION was found in dat.
           \nUse function st_filter_growing_seasons()")
    }

  }


  # table to use to filter and label growing seasons
  season_filters <- identify_growing_seasons(
    dat,
    trend_threshold = trend_threshold,
    superchill_threshold = superchill_threshold,
    max_season = max_season
  )

  # seasons to loop over
  seasons_unique <- unique(season_filters$SEASON)

  # empty list to store output from loops
  seasons_dat <- list()

  # index for storing output in the list (so only need one list for nested loops)
  k <- 0

  # loop over seasons
  for(i in seq_along(seasons_unique)){

    season.i <- seasons_unique[i]

    filters.i <- filter(season_filters, SEASON == season.i)

    depths.i <- unique(filters.i$DEPTH)

    # loop over depths
    for(j in seq_along(depths.i)){

      k <- k + 1

      depth.j <- depths.i[j]

      dat.j <- dat %>%
        filter(DEPTH == depth.j) %>%
        mutate(SEASON = season.i)

      filters.ij <-  filters.i %>%
        filter(DEPTH == depth.j) %>%
        select(-DEPTH, -SEASON) %>%
        mutate(START_SEASON = if_else(
          is.na(START_SEASON), min(dat.j$TIMESTAMP), START_SEASON)
        )

      seasons_dat[[k]] <- setDT(dat.j)[TIMESTAMP %inrange% filters.ij]

    } # end loop over depths

  } # end loop over seasons

  filtered_final <- seasons_dat %>% map_df(rbind)

}
