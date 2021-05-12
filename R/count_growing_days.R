#' Count number of days that were suitable for growth
#'
#' @inheritParams identify_heat_stress_events
#' @param ... Additional grouping variables (beside \code{DEPTH}).
#' @return Returns a tibble with columns: DEPTH, START_SEASON (minimum TIMESTAMP
#'   for each group), END_SEASON (maximum TIMESTAMP for each group), TOTAL_DAYS
#'   \code{difftime(END_SEASON, START_SEASON, units = "days")}, n_filtered_days
#'   (calculated from \code{identify_heat_stress_events}), and n_growing_days
#'   (TOTAL_DAYS - n_filtered_days).
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
                               heat_threshold = 18,
                               n_hours = 24){

  # number of days filtered out due to heat stress events
  filtered_days <- identify_heat_stress_events(dat = dat,
                                               heat_threshold = heat_threshold,
                                               n_hours = n_hours) %>%
    mutate(
      n_filtered_days = as.numeric(
        difftime(stress_end, stress_start, units = "day")
      )
    ) %>%
    group_by(DEPTH, ...) %>%
    summarize(n_filtered_days = sum(n_filtered_days))


 dat %>%
    group_by(DEPTH, ...) %>%
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
    )

}