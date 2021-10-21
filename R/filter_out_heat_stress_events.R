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
#' @importFrom dplyr filter select arrange mutate
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

    stations <- unique(dat$STATION)

  } else stations <- NULL


  if("SEASON" %in% colnames(dat)){

    seasons <- unique(dat$SEASON)

  } else seasons <- NULL


  # store filtered data in a list to speed up loop
  st_dat_filtered <- list()


  # Loop over STATION ------------------------------------------------------

  if(!is.null(stations) & is.null(seasons)){

    for(i in seq_along(stations)){

      station.i <- stations[i]

      dat.i <- dat %>%
        filter(STATION == station.i) %>%
        select(-STATION)

      st_dat_filtered[[i]] <- filter_out_heat_stress_events_single(
        dat = dat.i,
        heat_threshold = heat_threshold,
        n_hours = n_hours
      ) %>%
        mutate(STATION = station.i)

    }

    st_dat_filtered %>%
      map_df(rbind) %>%
      arrange(STATION, TIMESTAMP)

  }

  # Loop over SEASON --------------------------------------------------------

  if(is.null(stations) & !is.null(seasons)){

    # loop over each STATION and remove observations included in heat_stress_events
    for(i in seq_along(seasons)){

      season.i <- seasons[i]

      dat.i <- dat %>%
        filter(SEASON == season.i) %>%
        select(-SEASON)

      st_dat_filtered[[i]] <- filter_out_heat_stress_events_single(
        dat = dat.i,
        heat_threshold = heat_threshold,
        n_hours = n_hours
      ) %>%
      mutate(SEASON = season.i)

    }

    st_dat_filtered %>%
      map_df(rbind) %>%
      arrange(SEASON, TIMESTAMP)
  }

  # Loop over STATION and SEASON --------------------------------------------

  if(!is.null(stations) & !is.null(seasons)){

    st_dat_filtered_j <- list()

    for(i in seq_along(stations)){

      station.i <- stations[i]

      dat.i <- filter(dat, STATION == station.i)

      season.i <- unique(dat.i$SEASON)

      for(j in seq_along(season.i)) {

        season.j <- season.i[j]

        dat.j <- dat.i %>%
          filter(SEASON == season.j) %>%
          select(-STATION, -SEASON)

        st_dat_filtered_j[[j]] <- filter_out_heat_stress_events_single(
          dat = dat.j,
          heat_threshold = heat_threshold,
          n_hours = n_hours
        ) %>%
          mutate(STATION = station.i, SEASON = season.j)

      } # end loop over seasons

      st_dat_filtered[[i]] <- st_dat_filtered_j %>% map_df(rbind)

    } # end loop over stations

   st_dat_filtered %>%
     map_df(rbind) %>%
     arrange(STATION, SEASON, TIMESTAMP)

  } # end if statement

}









