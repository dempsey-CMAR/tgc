#' @title Calculates temperature in degree-days
#' @details Degree-days = average temperature over \emph{n} days * \emph{n} days
#'
#'   \emph{n} is calculated as ...
#'
#'
#' @param dat Dataframe with at least three columns: \code{TIMESTAMP} (must be
#'   possible to convert to a Date object), \code{DEPTH}, and \code{VALUE}. If
#'   column \code{VARIABLE} is included, it must have one unique entry. May also
#'   include columns with grouping variables passed to \code{...}. Other columns
#'   will be ignored.
#'
#' @param ... Additional columns in \code{dat} to use as grouping variables.
#'   Results are automatically grouped by \code{DEPTH}.
#'
#' @param heat_threshold The threshold to trigger heat stress interval
#'   (inclusive. Default is \code{heat_threshold = 18}).
#'
#' @param n_hours Length of heat stress interval in hours (default is
#'   \code{n_hours = 24}).
#'
#' @return Returns a tibble with at least five columns: \code{PERIOD} (start and
#'   end date used to calculate mean temperature and number of days),
#'   \code{n_DAYS} (the number of days, \emph{n}, used in the calculation),
#'   \code{n_OBSERVATIONS} (the number of observations used to calculate the
#'   average temperature), \code{AVG_TEMPERATURE} (the average temperature in
#'   the time period), \code{DEGREE_DAYS} (degree-days, the product of
#'   \code{AVG_TEMPERATURE} and \code{n_DAYS}). Additional columns are returned
#'   for each grouping variable in \code{...}.

#' @family calculate
#' @author Danielle Dempsey
#' @import dplyr
#' @importFrom lubridate date parse_date_time
#' @export


count_degree_days <- function(dat,
                              ...,
                              heat_threshold = 18,
                              n_hours = 24){

  if("VARIABLE" %in% colnames(dat)){

    dat <- filter(dat, VARIABLE == "Temperature")
  }

  growing_days <- count_growing_days(dat = dat,
                                     heat_threshold = heat_threshold,
                                     n_hours = n_hours)

  # calculate and return degree-days
  dat %>%
    # group by the columns specified in ...
    group_by(..., DEPTH) %>%
    mutate(DATE = date(TIMESTAMP)) %>%
    summarise(
      START_DAY = format(min(DATE), "%Y-%b-%d"),
      END_DAY = format(max(DATE), "%Y-%b-%d"),
      # number of observations in each group
      n_OBSERVATIONS = n(),
      # number of days in group
      n_DAYS = length(unique(DATE)),
      # average temperature in group
      AVG_TEMPERATURE =  round(mean(VALUE), digits = 3)) %>%
    mutate(DEGREE_DAYS = round(n_DAYS * AVG_TEMPERATURE, digits = 0)) %>%
    ungroup() %>%
    select(START_DAY, END_DAY, n_DAYS, everything()) %>%
    arrange(parse_date_time(START_DAY, orders = "Ymd"))

}









