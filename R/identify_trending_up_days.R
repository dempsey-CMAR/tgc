#' Identifies when VALUE begins trending up
#' @details Identifies timestamp when value crosses threshold and does not
#'   return below the threshold, for each group in \code{...}.
#'
#' @param lower_threshold Default is \code{lower_threshold = 4}.
#' @inheritParams calculate_degree_days
#' @return Returns the TIMESTAMP (for each group in \code{...}) for the final
#'   time VALUE exceeds \code{lower_threshold} and does not return below
#'   \code{lower_threshold}.
#'
#'   No row will be returned for groups for which VALUE did not cross
#'   \code{lower_threshold}.
#'
#' @importFrom dplyr arrange mutate filter summarise ungroup
#' @importFrom lubridate year
#' @export
#'
#' @examples
#' data("string_data")
#'
#' string_data <- string_data[which(string_data$VARIABLE == "Temperature"), ]
#' trend_up <- identify_trending_up_days(string_data,lower_threshold = 4, DEPTH)



identify_trending_up_days <- function(dat, ..., lower_threshold = 4){


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
