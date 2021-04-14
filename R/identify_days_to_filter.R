#' Identifies days during which threshold is exceeded
#'
#' @description Identifies days when the value exceeds a given threshold for a
#'   given number of observations. Option to identify additional days after the
#'   threshold is exceeded.
#'
#'   \code{dat} must only include data for one variable.
#'
#' @param dat Dataframe with at least three columns: \code{TIMESTAMP} (must be
#'   possible to convert to a Date object), \code{DEPTH}, and \code{VALUE}. If
#'   column \code{VARIABLE} is included, it must have one unique entry. Other
#'   columns will be ignored.
#'
#' @param threshold The threshold to trigger filtering data (inclusive. Default
#'   is \code{threshold = 20}).
#' @param min_exceedance The minimum number of observations in a day to trigger
#'   filtering data (inclusive). Default is \code{min.exceedence = 1}.
#' @param n_extra_days Number of additional days to filter out when
#'   \code{threshold} and \code{min_exceedence} are exceeded. If
#'   \code{n_extra_days = 0} (the default), then all values from the day the
#'   threshold is exceeded will be filtered out. If \code{n_extra_days = 1},
#'   then all values from the day the threshold is exceeded and the following
#'   day will be filtered out, etc.
#'
#' @return Returns table of dates that should be filtered based on threshold,
#'   min_exceedance, and n_extra_days
#'
#' @importFrom dplyr mutate filter if_else group_by summarise
#' @importFrom lubridate as_date days
#'
#' @export


identify_days_to_filter <- function(dat,
                                    threshold = 20,
                                    min_exceedance = 1,
                                    n_extra_days = 0){


  if("VARIABLE" %in% colnames(dat)){

    if(length(unique(dat$VARIABLE)) > 1) {

      stop("More than one VARIABLE found in dat. \n
         HINT: filter dat for the variable of interest before applying
         function")
    }
  }

  # table of DEPTH and the DATE for which the threshold is exceeded
  to_remove1 <- dat %>%
    mutate(
      DATE = lubridate::as_date(TIMESTAMP),
      EXCEED_THRESH = if_else(VALUE >= threshold, TRUE, FALSE)
    ) %>%
    group_by(DEPTH, DATE) %>%
    summarise(n_obs = sum(EXCEED_THRESH)) %>%
    filter(n_obs >= min_exceedance)

  # copy the rows of the table and add n.extra days to each date
  to_remove1 %>%
    rbind(
      to_remove1 %>% mutate(DATE = DATE + lubridate::days(n_extra_days))
    ) %>%
    distinct(DATE, DEPTH)

}
