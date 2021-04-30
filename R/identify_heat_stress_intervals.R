#' Identifies n-hour intervals beginning when threshold is exceeded
#'
#' @param dat Dataframe with at least three columns: \code{TIMESTAMP} (POSIXct
#'   or character in the order year, month, day, hours, minutes, seconds),
#'   \code{DEPTH}, and \code{VALUE}. If column \code{VARIABLE} is included, it
#'   must have one unique entry. Other columns will be ignored.
#'
#' @param heat_threshold The threshold to trigger heat stress interval
#'   (inclusive. Default is \code{heat_threshold = 18}).
#'
#' @param n_hours Length of heat stress interval in hours (default is
#'   \code{n_hours = 24}).
#'
#' @return Returns a dataframe with three columns: \code{DEPTH},
#'   \code{interval_start} and \code{interval_end}. \code{interval_start}
#'   indicates the beginning of each heat stress interval, i.e, the
#'   \code{TIMESTAMP} of all observations where \code{VALUE} is greater than or
#'   equal to \code{threshold}. \code{interval_end} identifies the end of the
#'   heat stress interval, i.e., \code{interval_start} + n_hours.
#'
#'   Note: intervals may overlap with previous interval(s).
#'
#' @importFrom dplyr mutate filter select
#' @importFrom lubridate as_datetime hours
#'
#' @family heat stress
#'
#' @export
#'
#' @examples
#'
#' data(string_data)
#'
#' dat <- string_data[which(string_data$VARIABLE == "Temperature"), ]
#' stress_intervals <- identify_heat_stress_intervals(dat)



identify_heat_stress_intervals <- function(dat,
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
    select(DEPTH, interval_start, interval_end, -EXCEED_THRESH)

}
