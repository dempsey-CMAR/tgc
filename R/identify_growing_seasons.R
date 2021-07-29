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
#'   \code{months(max_season)}.
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
#' @return Returns a tibble with the \code{START_SEASON} and \code{END_SEASON}
#'   for each group in \code{DEPTH} and group in \code{...}.
#'
#' @importFrom lubridate as_date minutes year month days
#' @importFrom dplyr filter full_join mutate group_by arrange select
#' @importFrom purrr map_df
#'
#' @export


identify_growing_seasons <- function(dat,
                                     ...,
                                     trend_threshold = 4,
                                     superchill_threshold = -0.7,
                                     max_season = 540){

  dat <- dat %>%
    mutate(YEAR = lubridate::year(TIMESTAMP),
           MONTH = lubridate::month(TIMESTAMP))

  # to make sure all groups get assigned start and end dates
  season_table <- dat %>%
    group_by(..., YEAR, DEPTH) %>%
    summarize(MIN_TIMESTAMP = min(TIMESTAMP))

  season_start <- dat %>%
    # filter to avoid when Temperature crosses the threshold
    # after a hurricane (Sept, Oct) or when trending down (Nov - Dec)
    filter(MONTH <= 8) %>%
    identify_trending_up(YEAR, ...,
                         trend_threshold = trend_threshold)

  season_end <- dat %>%
    # filter to ensure identifying Spring superchill (not winter cold temps)
    filter(MONTH <= 5) %>%
    identify_first_superchill(YEAR, ...,
                              superchill_threshold = superchill_threshold) %>%
    # to merge with season_start
    mutate(YEAR = YEAR - 1)

  # join by YEAR and columns in ...
  season_table <- season_table %>%
    full_join(season_start) %>%
    full_join(season_end) %>%
    mutate(
      # when there is an END_SEASON (from superchill) but no start season
      ## (no data from the year before, e.g., Madeline Point)
      MIN_TIMESTAMP = case_when(is.na(MIN_TIMESTAMP) ~ min(dat$TIMESTAMP),
                                TRUE ~ MIN_TIMESTAMP),
      # if temperature never crosses 4-degree threshold, use minimum TIMESTAMP
      START_SEASON = case_when(is.na(START_TREND) ~ MIN_TIMESTAMP,
        TRUE ~ START_TREND
      ),
      # if temperature never crosses superchill threshold, use set duration for season
      END_SEASON = case_when(
        is.na(FIRST_CHILL) ~ START_SEASON + days(max_season),
        TRUE ~ as_datetime(FIRST_CHILL)
      )
    ) %>%
    group_by(..., YEAR) %>%
    mutate(ID = cur_group_id(),
           SEASON = paste0("S", ID)) %>%  # does not reset to 1 for each STATION; see loop below
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

  season_table


}
