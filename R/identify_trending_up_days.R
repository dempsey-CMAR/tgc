#' Identifies when VALUE begins trending up
#' @details Identifies timestamp when value crosses threshold and does not
#'   return below the threshold.
#'
#' @param lower_threshold Default is \code{lower_threshold = 4}
#' @inheritParams calculate_degree_days

#' @importFrom dplyr arrange mutate filter summarise ungroup
#' @importFrom lubridate year
#' @export


identify_trending_up_days <- function(dat, lower_threshold = 4, ...){


  if("VARIABLE" %in% colnames(dat)){

    if(length(unique(dat$VARIABLE)) > 1) {

      stop("More than one VARIABLE found in dat. \n
         HINT: filter dat for the variable of interest before applying
         function")
    }
  }

  dat %>%
    mutate(YEAR = year(TIMESTAMP)) %>%
    group_by(...) %>%
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    mutate(CROSS_THRESH = if_else(
      lag(VALUE) < lower_threshold & VALUE >= lower_threshold, TRUE, FALSE )
    ) %>%
    filter(CROSS_THRESH) %>%
    summarise(START_TREND = max(TIMESTAMP)) %>%
    ungroup()

}
