#' Identify when VALUE begins trending up
#'
#' @details Identifies \code{TIMESTAMP} when \code{VALUE} exceeds a threshold
#'   and does not return below the threshold, for each group in \code{DEPTH} and
#'   \code{...}.
#'
#'   If \code{VALUE} never crosses the threshold because temperature is always >
#'   \code{trend_threshold}, the date will be returned as \code{NA}.
#'
#'   \code{VALUE = trend_threshold} is considered over the threshold.
#'
#' @param dat Data frame with at least three columns: \code{TIMESTAMP} (must be
#'   possible to convert to POSIXct), \code{DEPTH}, and \code{VALUE}. If column
#'   \code{VARIABLE} is included, it must have one unique entry. May also
#'   include columns with grouping variables passed to \code{...}. Other columns
#'   will be ignored.
#'
#' @param ... Additional columns in \code{dat} to use as grouping variables.
#'   Results are automatically grouped by \code{DEPTH}.
#'
#' @param trend_threshold The threshold for "trending up". Default is
#'   \code{trend_threshold = 4}. The last observation above
#'   \code{trend_threshold} that does not return below \code{trend_threshold}
#'   triggers the beginning of the growing season for each \code{DEPTH} and
#'   group in \code{...}.
#'
#' @return Returns a tibble. \code{START_TREND} is the \code{TIMESTAMP} of the
#'   final time \code{VALUE} exceeds \code{trend_threshold} and does not return
#'   below \code{trend_threshold} (for each \code{DEPTH} and group in
#'   \code{...}).
#'
#'   This \code{TIMESTAMP} is passed to \code{identify_growing_seasons()} to
#'   denote the start of the growing season.
#'
#'   \code{START_TREND} is assigned \code{NA} for groups for which \code{VALUE}
#'   did not cross \code{trend_threshold}.
#'
#' @importFrom dplyr arrange mutate filter summarise group_by ungroup left_join
#' @importFrom lubridate year as_datetime
#' @export
#'
#' @examples
#' data("string_data")
#'
#' string_data <- string_data[which(string_data$VARIABLE == "Temperature"), ]
#' trend_up <- identify_trending_up(string_data, trend_threshold = 4)


identify_trending_up <- function(dat, ..., trend_threshold = 4){


  if("VARIABLE" %in% colnames(dat)){

    if(length(unique(dat$VARIABLE)) > 1) {

      stop("More than one VARIABLE found in dat. \n
         HINT: filter dat for the variable of interest before applying
         function")
    }
  }

  trend_groups <- dat %>%
    distinct(..., DEPTH)

  trend_table <- dat %>%
    mutate(
      TIMESTAMP = as_datetime(TIMESTAMP)#
     #YEAR = year(TIMESTAMP)
    ) %>%
    group_by(..., DEPTH) %>%
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    mutate(CROSS_THRESH = if_else(
      lag(VALUE) < trend_threshold & VALUE >= trend_threshold, TRUE, FALSE )
    ) %>%
    filter(CROSS_THRESH) %>%
    ungroup()

  # for stations that never cross the threshold (e.g., Cornwallis)
  if(nrow(trend_table) == 0){

    trend_table <- trend_groups %>%
      mutate(START_TREND = as_datetime(NA_character_))

  } else{

    trend_table <- trend_groups %>%
      left_join(trend_table) %>%
      group_by(..., DEPTH) %>%
      summarise(START_TREND = max(TIMESTAMP)) %>%
      ungroup()
  }

  trend_table

}







