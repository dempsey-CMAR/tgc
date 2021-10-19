#' Keep observations that occur during growing seasons
#'
#' @details Keeps observations that occur during growing seasons as defined by
#'   \code{identify_growing_seasons()} (i.e., filters out observations that do
#'   not occur during the growing seasons).
#'
#'   This function only works for a single \code{STATION}. For multiple
#'   \code{STATION}s, use \code{st_filter_in_growing_seasons()}.
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

filter_in_growing_seasons_single <- function(dat,
                                   full_season = TRUE,
                                   trend_threshold = 4,
                                   superchill_threshold = -0.7,
                                   max_season = 540){

  # check how many different STATIONS are included in dat
  if("STATION" %in% colnames(dat)){

    n_stations <- length(unique(dat$STATION))

    if(n_stations > 1){

      stop("More than one STATION was found in dat.
           \nUse function st_filter_in_growing_seasons()")
    }

  }

  # table to use to filter and label growing seasons
  season_filters <- identify_growing_seasons(
    dat,
    full_season = full_season,
    trend_threshold = trend_threshold,
    superchill_threshold = superchill_threshold,
    max_season = max_season
  ) %>%
    select(-SEASON_DAYS, -SEASON_MONTHS)

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

      # if START_SEASON or END_SEASON is NA, no rows will be returned
      filters.ij <-  filters.i %>%
        filter(DEPTH == depth.j) %>%
        select(-DEPTH, -SEASON)

      seasons_dat[[k]] <- setDT(dat.j)[TIMESTAMP %inrange% filters.ij]

    } # end loop over depths

  } # end loop over seasons

  filtered_final <- seasons_dat %>% map_df(rbind)

}
