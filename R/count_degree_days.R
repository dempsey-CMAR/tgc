#' @title Count number of degree-days
#' @details Degree-days = average temperature over \emph{n} days * \emph{n} days
#'
#'   \emph{n} is calculated as ...
#'
#'   dat is FILTERED DATA
#'
#'   Could put count_growing_days and apply_dd_filters in here
#'
#'
#' @param dat Dataframe with at least three columns: \code{TIMESTAMP} (must
#'   be possible to convert to a Date object), \code{DEPTH}, and \code{VALUE}.
#'   If column \code{VARIABLE} is included, it must have one unique entry. May
#'   also include columns with grouping variables passed to \code{...}. Other
#'   columns will be ignored.
#'
#' @param ... Additional columns in \code{dat} to use as grouping variables.
#'   Results are automatically grouped by \code{DEPTH}.
#'
#' @param growing_days Data.frame for each group
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


count_degree_days <- function(dat, ..., growing_days){

  if("VARIABLE" %in% colnames(dat)){

    dat <- filter(dat, VARIABLE == "Temperature")
  }

  if(!("n_growing_days" %in% colnames(growing_days))) {

    stop("argument growing days MUST include column n_growing_days.

        HINT: check spelling")
  }

  # count degree-day for each group
  dat %>%
    group_by(..., SEASON, DEPTH) %>%
    summarise(
      # number of observations in each group
      n_OBSERVATIONS = n(),
      # average temperature in group
      AVG_TEMPERATURE =  round(mean(VALUE), digits = 3)
    ) %>%
    left_join(growing_days) %>%
    mutate(
      n_degree_days = round(n_growing_days * AVG_TEMPERATURE, digits = 2)
    ) %>%
    ungroup() %>%
    select(..., SEASON, DEPTH,
           START_SEASON, END_SEASON, TOTAL_DAYS,
           n_filtered_days, n_growing_days, AVG_TEMPERATURE, n_degree_days)

}









