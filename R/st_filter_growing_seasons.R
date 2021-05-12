#' Keeps observations that occur during growing seasons for multiple stations
#'
#' @inheritParams identify_growing_seasons
#' @return Returns dat, filtered for observations that occur during growing
#'   seasons identified in \code{identify_growing_seasons}, with an additional
#'   column \code{SEASON} to label the seasons ("S1", "S2", ...). Some
#'   observations may be duplicated in consecutive seasons.

#' @export

st_filter_growing_seasons <- function(dat,
                                      trend_threshold = 4,
                                      superchill_threshold = -0.7,
                                      max_season = 18){

  stations <- unique(dat$STATION)

  # store filtered data in a list to speed up loop
  st_dat_filtered <- list()

  # loop over each STATION and label seasons
  for(i in seq_along(stations)){

    station.i <- stations[i]

   # print("st_filter_growing_seasons(): filtering station", station.i)

    dat.i <- filter(dat, STATION == station.i)

    st_dat_filtered[[i]] <- filter_growing_seasons(
      dat = dat.i,
      trend_threshold = trend_threshold,
      superchill_threshold = superchill_threshold,
      max_season = max_season
    )
  }

  st_dat_filtered %>% map_df(rbind)


}
