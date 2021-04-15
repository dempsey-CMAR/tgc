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
#'   min_exceedance, and n_extra_days. Three columns: DEPTH, DATE, and n_obs
#'   (the number of observations that exceeded the threshold at the DEPTH and
#'   DATE).
#'
#' @importFrom dplyr mutate filter if_else group_by summarise
#' @importFrom lubridate as_date days
#' @importFrom purrr map_df
#'
#' @export


identify_days_above_threshold <- function(dat,
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
  to_remove <- dat %>%
    mutate(
      DATE = as_date(TIMESTAMP),
      EXCEED_THRESH = if_else(VALUE >= threshold, TRUE, FALSE)
    ) %>%
    group_by(DEPTH, DATE) %>%
    summarise(n_obs = sum(EXCEED_THRESH)) %>%
    filter(n_obs >= min_exceedance) %>%
    ungroup()

  # add 1 day to each date, then add 2 days to each date, until n_extra_days
  if(n_extra_days > 0){

    to_remove_extra <- list()

    for(i in 1:n_extra_days){

      remove.i <- to_remove %>%
        mutate(DATE = DATE + lubridate::days(i))

      to_remove_extra[[i]] <- remove.i
    }

    # unlist and bind with original dates. Remove duplicates
    to_remove_extra <- to_remove_extra %>%
      purrr::map_df(rbind) %>%
      rbind(to_remove) %>%
      distinct(DATE, DEPTH) %>%
      arrange(DEPTH, DATE)

    # so thats n_obs is correct (NA for days without exceedances)
    to_remove <- to_remove_extra %>%
      left_join(to_remove, by = c("DEPTH", "DATE"))
  }

  to_remove

}
