#' @title Calculates temperature in degree-days
#' @details Degree-days = average temperature over \emph{n} days * \emph{n} days
#'
#'   \emph{n} is calculated as the number of unique dates in the TIMESTAMP
#'   column. This means that days without data (e.g. due to sensor mal-function
#'   or deliberately filtered out) will NOT be included in the degree-day
#'   calculation.For example: a sensor is deployed from September 1, 2021 to
#'   September 14, 2021 (14 days).
#'
#'   The temperature for several observations on September 3 was > 20 degree
#'   Celcius, and so that day is filtered out of the analysis. In the degree-day
#'   calculation \emph{n} = 13 days.
#'
#' @param dat Dataframe with at least three columns: \code{TIMESTAMP} (must be
#'   possible to convert to a Date object), \code{DEPTH}, and \code{VALUE}. If
#'   column \code{VARIABLE} is included, it must have one unique entry. May also
#'   include columns with grouping variables passed to \code{...}. Other columns
#'   will be ignored.
#'
#' @param start.date First day to include in the calculation. If
#'   \code{start.date} does not include a time, then the start time is assumed
#'   to be midnight. Accepted orders for \code{start.date} are: "ymd IMS p",
#'   "Ymd IMS p", "Ymd HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd".
#'   Default is the first TIMESTAMP in \code{dat}.
#' @param end.date Last day to include in the calculation. If \code{end.date}
#'   does not include a time, then the end time is assumed to be "23:59:59".
#'   Accepted orders for \code{end.date} are: "ymd IMS p", "Ymd IMS p", "Ymd
#'   HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is the last
#'   TIMESTAMP in \code{dat}.
#' @param ... Columns in \code{dat} to use for grouping in
#'   \code{dplyr::group_by()}, e.g., \code{YEAR, DEPTH}.
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
#'
#' @examples
#' data(string_data)
#'
#' # degree-days averaged over all depths and the whole time series
#' calculate_degree_days(string_data)
#'
#' # degree-days by DEPTH for whole time series
#' calculate_degree_days(string_data, DEPTH)
#'
#' # degree-days by DEPTH and month from July 1 to September 30
#' calculate_degree_days(string_data, DEPTH, months(TIMESTAMP),
#' start.date = "2019-07-01", end.date = "2019-09-30")

calculate_degree_days <- function(dat,
                                  ...,
                                  start.date = min(dat$TIMESTAMP),
                                  end.date = max(dat$TIMESTAMP)
                                  ){

  # format dates (convert to datetime object and add time if only date was supplied)
  start.date <- format_date_for_dd(start.date, day1 = TRUE)
  end.date <- format_date_for_dd(end.date, day1 = FALSE)

  if("VARIABLE" %in% colnames(dat)){

    dat <- filter(dat, VARIABLE == "Temperature")
  }

  # calculate and return degree-days
  dat %>%
    filter(TIMESTAMP >= start.date, TIMESTAMP <= end.date) %>%
    # group by the columns specified in ...
    group_by(...) %>%
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









