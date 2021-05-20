#' Count number of days that were suitable for growth
#'
#' @details Days suitable for growth are days that remain after applying season
#'   and heat stress filters (\code{apply_dd_filters}).
#'
#'   Results are automatically grouped by \code{SEASON} and \code{DEPTH}.
#'
#' @inheritParams apply_dd_filters
#'
#' @inheritParams identify_trending_up
#'
#' @param ... Additional columns in \code{dat} to use as grouping variables.
#'   Results are automatically grouped by \code{SEASON} and \code{DEPTH}.
#'
#' @return Returns a tibble with columns: \code{...}, \code{DEPTH},
#'   \code{SEASON}, \code{START_SEASON} (minimum TIMESTAMP for each group),
#'   \code{END_SEASON} (maximum TIMESTAMP for each group), \code{TOTAL_DAYS}
#'   (\code{difftime(END_SEASON, START_SEASON, units = "days")}),
#'   \code{n_filtered_days} (calculated from
#'   \code{identify_heat_stress_events}), and \code{n_growing_days} (TOTAL_DAYS
#'   - n_filtered_days).
#'
#' @importFrom dplyr mutate group_by summarize left_join if_else
#'
#' @export
#'
#' @examples
#' data(string_data)
#' string_data <- string_data[which(string_data$VARIABLE == "Temperature"), ]
#' count_growing_days(string_data)

count_growing_days <- function(dat,
                               ...,
                               trend_threshold = 4,
                               superchill_threshold = -0.7,
                               max_season = 18,
                               heat_threshold = 18,
                               n_hours = 24){

  # number of days filtered out due to heat stress events
  filtered_days <- identify_heat_stress_events(dat = dat,
                                               ...,
                                               heat_threshold = heat_threshold,
                                               n_hours = n_hours) %>%
    mutate(
      n_filtered_days = as.numeric(
        difftime(stress_end, stress_start, units = "day")
      )
    ) %>%
    group_by(..., DEPTH) %>%
    summarize(n_filtered_days = sum(n_filtered_days)) %>%
    ungroup()

  if("STATION" %in% colnames(dat)){

    dat_out <- dat %>%
      st_filter_growing_seasons(trend_threshold = trend_threshold,
                                superchill_threshold = superchill_threshold,
                                max_season = max_season)
  } else{

    dat_out <- dat %>%
      filter_growing_seasons(trend_threshold = trend_threshold,
                             superchill_threshold = superchill_threshold,
                             max_season = max_season)
  }

  dat_out %>%
    group_by(..., SEASON, DEPTH) %>%
    summarise(
      START_SEASON = min(TIMESTAMP),
      END_SEASON = max(TIMESTAMP)
    ) %>%
    mutate(
      TOTAL_DAYS = as.numeric(
        difftime(END_SEASON, START_SEASON, units = "days")
      )
    ) %>%
    left_join(filtered_days) %>%
    mutate(
      n_filtered_days = if_else(is.na(n_filtered_days), 0, n_filtered_days),
      n_growing_days = round(TOTAL_DAYS - n_filtered_days, digits = 2)
    ) %>%
    ungroup()

}
