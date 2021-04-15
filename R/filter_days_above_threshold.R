#' Filters out data from days when the value exceeds a threshold
#'
#' @description Filters out all data from days when the value exceeds a given
#'   threshold for a given number of observations. Option to filter out
#'   additional days after the threshold is exceeded.
#'
#'   \code{dat.tidy } must only include data for one variable.
#'
#' @inheritParams identify_days_above_threshold
#' @return Returns dat, filtered when triggered by \code{threshold} and
#'   \code{min.exceedence}.
#'
#' @importFrom dplyr select anti_join
#' @importFrom lubridate as_date
#'
#' @export

filter_days_above_threshold <- function(dat,
                        threshold = 20,
                        min_exceedance = 1,
                        n_extra_days = 0) {

  to_remove <- identify_days_above_threshold(
    dat = dat,
    threshold = threshold,
    min_exceedance = min_exceedance,
    n_extra_days
  )

  # anti-join to remove the rows in to_remove from dat.tidy
  dat %>%
    mutate(DATE = as_date(TIMESTAMP)) %>%
    anti_join(to_remove, by = c("DEPTH", "DATE")) %>%
    select(-DATE)

}
