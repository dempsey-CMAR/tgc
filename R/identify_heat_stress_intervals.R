#' Identify n-hour intervals beginning when a threshold is exceeded
#'
#' @inheritParams identify_trending_up
#'
#' @param heat_threshold The threshold for heat stress. Default is
#'   \code{heat_threshold = 18}). Every observation above \code{heat_threshold}
#'   triggers an \code{n-hour} heat stress interval.
#'
#' @param n_hours Length of heat stress interval in hours (default is
#'   \code{n_hours = 24}).
#'
#' @return Returns a data frame with columns: \code{...},  \code{DEPTH},
#'   \code{interval_start} and \code{interval_end}. \code{interval_start}
#'   indicates the beginning of each heat stress interval, i.e, the
#'   \code{TIMESTAMP} of all observations where \code{VALUE} is greater than or
#'   equal to \code{heat_threshold}. \code{interval_end} identifies the end of
#'   the heat stress interval, i.e., \code{interval_start} + n_hours.
#'
#'   Note: intervals may overlap with previous interval(s).
#'
#' @importFrom dplyr mutate filter select arrange %>%
#' @importFrom lubridate as_datetime hours
#'
#' @family heat stress
#'
#' @export
#'
#' @examples
#' data(string_data)
#'
#' dat <- string_data[which(string_data$VARIABLE == "Temperature"), ]
#' stress_intervals <- identify_heat_stress_intervals(dat)



identify_heat_stress_intervals <- function(dat,
                                           ...,
                                           heat_threshold = 18,
                                           n_hours = 24){


  if("VARIABLE" %in% colnames(dat)){

    if(length(unique(dat$VARIABLE)) > 1) {

      stop("More than one VARIABLE found in dat. \n
         HINT: filter dat for the variable of interest before applying
         function")
    }
  }

  dat %>%
    mutate(
      TIMESTAMP = as_datetime(TIMESTAMP),
      EXCEED_THRESH = VALUE >= heat_threshold
    ) %>%
    filter(EXCEED_THRESH) %>%
    mutate(
      interval_start = TIMESTAMP,
      interval_end = TIMESTAMP + hours(n_hours)
    ) %>%
    select(..., DEPTH, interval_start, interval_end, -EXCEED_THRESH) %>%
    arrange(..., DEPTH, interval_start)

}
