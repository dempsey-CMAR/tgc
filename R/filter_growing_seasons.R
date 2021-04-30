#' Keeps observations that occur during growing seasons
#'
#' @details
#'
#' for ONE STATION
#'
#'
#' @inheritParams identify_growing_seasons
#' @return Returns dat, filtered for observations that occur during growing
#'   seasons identified in \code{identify_growing_seasons}, with an additional
#'   column \code{SEASON} to label the seasons ("S1", "S2", ...). Some
#'   observations may be duplicated in consecutive seasons.

#' @export

filter_growing_seasons <- function(dat,
                                  ...,
                                  trend_threshold = 4,
                                  superchill_threshold = -0.7,
                                  max_season = 18){

  # table to use to filter and label growing seasons
  season_filters <- identify_growing_seasons(
    dat,
    ...,
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
