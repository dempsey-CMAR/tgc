#' @title Count number of degree-days
#'
#' @details Degree-days = average temperature over \emph{n} days * \emph{n} days
#'
#'   \emph{n} is the number of days suitable for growth (from
#'   \code{count_growing_days()}).
#'
#'   Results are grouped by \code{SEASON}, \code{DEPTH}, and \code{...}.
#'
#'
#'  ***Could put count_growing_days and apply_dd_filters in here**
#'
#' @param ... Additional columns in \code{dat} to use as grouping variables.
#'   Results are automatically grouped by \code{SEASON} and \code{DEPTH}.
#'
#' @param dat_filt Dataframe that has been filtered by
#'   \code{apply_dd_filters()}. Includes columns: \code{...}, \code{SEASON},
#'   \code{DEPTH}, and \code{VALUE}. If column \code{VARIABLE} is included, it
#'   must have one unique entry. May also include columns with grouping
#'   variables passed to \code{...}. Other columns will be ignored.
#'
#' @param growing_days Data.frame for each group
#'
#' @return Returns a tibble with columns:

#' @family calculate
#' @author Danielle Dempsey
#' @import dplyr
#' @importFrom lubridate date parse_date_time
#' @export


count_degree_days <- function(dat_filt, ..., growing_days){

  if("VARIABLE" %in% colnames(dat_filt)){

    dat_filt <- filter(dat_filt, VARIABLE == "Temperature")
  }

  if(!("n_growing_days" %in% colnames(growing_days))) {

    stop("argument growing days MUST include column n_growing_days.

        HINT: check spelling")
  }

  # count degree-day for each group
  dat_filt %>%
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
           START_SEASON, END_SEASON, STOCKED_DAYS,
           n_filtered_days, n_growing_days, AVG_TEMPERATURE, n_degree_days)

}









