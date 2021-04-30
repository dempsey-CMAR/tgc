#' Identifies start and end of growing seasons for each year
#'
#' @details For each group in \code{...}, the growing season starts when the
#'   temperature crosses \code{trend_threshold} and does not return below
#'   \code{trend_threshold} (e.g., 4-degrees trending up) and ends 1 minute
#'   before the first observation of \code{superchill_threshold}.
#'
#'   The function looks for \code{START_SEASON} in January to August to avoid
#'   temperature drops caused by hurricanes (typically in September and October)
#'   and Winter cooling (November, December).
#'
#'   The function looks for \code{END_SEASON} in January to May of the next year
#'   (spring superchill).
#'
#'   If the time series begins when the temperature is above
#'   \code{trend_threshold}, \code{START_SEASON} will be \code{NA}.
#'   \code{START_SEASON} will be assigned the minimum \code{TIMESTAMP} for the
#'   group in \code{filter_growing_season()}.
#'
#'   If the time series does not go below \code{superchill_threshold}, the
#'   growing season is assumed to be \code{max_season} months long.
#'   \code{END_SEASON} is assigned the the timestamp \code{START_SEASON} +
#'   \code{months(max_season)}.
#'
#'   Growing seasons may overlap if \code{max_season} is used to determine
#'   \code{END_SEASON}.
#'
#'   If \code{START_SEASON} and \code{END_SEASON} are both \code{NA} for a
#'   group, there will be no row for this group in the output.
#'

#' @inheritParams identify_trending_up
#' @inheritParams identify_first_superchill
#' @param max_season The length of the growing season in months for groups that
#'   have no temperature observations below \code{superchill_threshold}.

#' @return Returns a tibble with the \code{START_SEASON} and \code{END_SEASON}
#'   for each group in \code{...}.
#'
#' @importFrom lubridate as_date minutes year month
#'@importFrom dplyr filter full_join mutate group_by arrange select
#'
#' @export


identify_growing_seasons <- function(dat,
                                     ...,
                                     trend_threshold = 4,
                                     superchill_threshold = -0.7,
                                     max_season = 18){

  dat <- dat %>%
    mutate(YEAR = lubridate::year(TIMESTAMP),
           MONTH = lubridate::month(TIMESTAMP))

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
  season_start %>%
    full_join(season_end) %>%
    mutate(
      START_SEASON = START_TREND,
      END_SEASON = case_when(
        is.na(FIRST_CHILL) ~ START_TREND + months(max_season), TRUE ~ FIRST_CHILL
      )
    ) %>%
    group_by(YEAR) %>%
    mutate(SEASON = cur_group_id(),
           SEASON = paste0("S", SEASON)) %>%
    ungroup() %>%
    select(SEASON, ..., START_SEASON, END_SEASON) %>%
    arrange(SEASON)

}
