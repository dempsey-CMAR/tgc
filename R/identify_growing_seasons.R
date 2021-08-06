#' Identify start and end of growing seasons for each year
#'
#' @details The growing season starts when the temperature crosses
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
#'   If the time series begins when the temperature is above
#'   \code{trend_threshold}, \code{START_SEASON} is assigned the minimum
#'   \code{TIMESTAMP} for the group in \code{filter_growing_season()}.
#'
#'   If the time series does not go below \code{superchill_threshold}, the
#'   growing season is assumed to be \code{max_season} months long.
#'   \code{END_SEASON} is assigned the timestamp \code{START_SEASON} +
#'   \code{days(max_season)}.
#'
#'   Growing seasons may overlap if \code{max_season} is used to determine
#'   \code{END_SEASON}.
#'
#' @inheritParams identify_trending_up
#'
#' @inheritParams identify_first_superchill
#'
#' @param max_season The length of the growing season in months for groups that
#'   have no temperature observations below \code{superchill_threshold}. Default
#'   is \code{max_season = 540} days (~18 months). Note: units are days because
#'   adding 18 months to August 30 or August 31 results in \code{NA} (because
#'   February 30 and February 31 are not real dates).
#'
#' @param full_season Logical argument. The default, \code{full_season = TRUE}
#'   will return \code{NA} for \code{START_SEASON} and \code{END_SEASON} that
#'   occur outside of the date range of the data series. For example, if the
#'   data series starts when temperature is already above
#'   \code{trend_threshold}, \code{START_SEASON = NA}; if the season length is
#'   determined by \code{max_season}, which results in a date after the last
#'   observation, \code{END_SEASON = NA}. If \code{full_season = FALSE}, the
#'   \code{START_SEASON} and \code{END_SEASON} dates default to the first and
#'   last timestamp in \code{...} and \code{DEPTH}.
#'
#' @return Returns a tibble with the \code{START_SEASON}, \code{END_SEASON},
#'   \code{SEASON_DAYS} (length of season in days), and  \code{SEASON_MONTHS}
#'   (season length in months; assumes 1 month = 30 days) for each group in
#'   \code{DEPTH} and group in \code{...}.
#'
#' @importFrom lubridate as_date minutes year month days
#' @importFrom dplyr filter full_join mutate group_by arrange select
#' @importFrom purrr map_df
#'
#' @export


identify_growing_seasons <- function(dat,
                                     ...,
                                     full_season = TRUE,
                                     trend_threshold = 4,
                                     superchill_threshold = -0.7,
                                     max_season = 540){

  dat <- dat %>%
    mutate(YEAR = lubridate::year(TIMESTAMP),
           MONTH = lubridate::month(TIMESTAMP))

  # if start/end season are outside of data series range, assign to first/last timestamp
  season_table <- dat %>%
    group_by(..., DEPTH) %>%
    # max timestamp for each STATION & DEPTH
    # (to constrain end date for final season without superchill)
    mutate(MAX_TIMESTAMP = max(TIMESTAMP)) %>%
    ungroup() %>%
    group_by(..., YEAR, DEPTH, MAX_TIMESTAMP) %>%
    # min timestamp for each STATION, YEAR & DEPTH
    # (optional start date for seasons with data series that begins above trend_thresh)
    summarize(MIN_TIMESTAMP = min(TIMESTAMP)) %>%
    ungroup()

  # filter to avoid when Temperature crosses the threshold
  # after a hurricane (Sept, Oct) or when trending down (Nov - Dec)
  season_start <- dat %>%
    filter(MONTH <= 8) %>%
    identify_trending_up(YEAR, ..., trend_threshold = trend_threshold)

  # filter to ensure identifying Spring superchill (not winter cold temps)
  season_end <- dat %>%
    filter(MONTH <= 5) %>%
    identify_first_superchill(YEAR, ...,
                              superchill_threshold = superchill_threshold) %>%
    # to merge with season_start
    mutate(YEAR = YEAR - 1) %>%
    select(YEAR, ..., DEPTH, FIRST_CHILL)


  season_table <- season_table %>%
    # this could be a left_join (?)
    full_join(season_start) %>%
    # this needs to be a full_join for data series that begin at the END of a season
    full_join(season_end) %>%
    group_by(...) %>%
    # assign MIN_ and MAX_ TIMESTAMP values to NAs resulting from the joins
    # e.g., when time series starts at the END of a season (Tickle Island)
    mutate(
      MIN_TIMESTAMP_NA = min(MIN_TIMESTAMP, na.rm = TRUE),

      MIN_TIMESTAMP = if_else(
        is.na(MIN_TIMESTAMP), MIN_TIMESTAMP_NA, MIN_TIMESTAMP
      ),

      MAX_TIMESTAMP_NA = max(MAX_TIMESTAMP, na.rm = TRUE),

      MAX_TIMESTAMP = if_else(
        is.na(MAX_TIMESTAMP), MAX_TIMESTAMP_NA, MAX_TIMESTAMP
      )
    ) %>%
    select(-MIN_TIMESTAMP_NA, -MAX_TIMESTAMP_NA) %>%
    ungroup() %>%
    mutate(
      START_SEASON = START_TREND,
      # if temperature never crosses superchill threshold, use set duration for season
      END_SEASON = if_else(
        is.na(FIRST_CHILL), START_SEASON + days(max_season), as_datetime(FIRST_CHILL)
      )
    )

  if(isTRUE(full_season)){
    # if END_SEASON is out of the max data range, assign NA
    season_table <- season_table %>%
      mutate(END_SEASON = if_else(
        END_SEASON > MAX_TIMESTAMP, as_datetime(NA_character_), END_SEASON)
      )
  }

  if(isFALSE(full_season)){

    season_table <- season_table %>%
      mutate(
        # if temperature never crosses 4-degree threshold, use minimum TIMESTAMP
        START_SEASON = if_else(
          is.na(START_TREND), MIN_TIMESTAMP, START_TREND
        ),
        END_SEASON = if_else(
          is.na(FIRST_CHILL), START_SEASON + days(max_season), as_datetime(FIRST_CHILL)
        ),
        # if END_SEASON is out of the max data range, assign the max TIMESTAMP
        END_SEASON = if_else(
          END_SEASON > MAX_TIMESTAMP, MAX_TIMESTAMP, END_SEASON)
        )
  }

  # label seasons based on group ID (does not reset to S1 for each STATION; see loop below)
  season_table <- season_table %>%
    # na.omit() %>%
    group_by(..., YEAR) %>%
    mutate(ID = cur_group_id(),
           SEASON = paste0("S", ID)) %>%
    ungroup() %>%
    select(..., SEASON, DEPTH, START_SEASON, END_SEASON) %>%
    arrange(..., SEASON, DEPTH)


  # loop over each STATION and assign seasons starting at S1
  if("STATION" %in% colnames(dat)){

    stations <- unique(dat$STATION)

    if(length(stations) > 1) {

      table_out <- list()

      for(i in seq_along(stations)){

        table_out[[i]] <- season_table %>%
          filter(STATION == stations[i]) %>%
          group_by(SEASON) %>%
          mutate(ID = cur_group_id(), SEASON = paste0("S", ID)) %>%
          select(-ID) %>%
          ungroup()
      }

      season_table <- table_out %>% map_df(rbind)
    }

  }

  season_table %>%
    mutate(
    SEASON_DAYS = difftime(END_SEASON, START_SEASON, units = "days"),
    SEASON_DAYS = as.numeric(round(SEASON_DAYS, digits = 2)),
    SEASON_MONTHS = round(SEASON_DAYS / 30, digits = 2)
  )

}
