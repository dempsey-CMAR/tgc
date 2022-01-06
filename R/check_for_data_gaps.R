#' Check for substantial gaps in the time series
#'
#' @inheritParams identify_trending_up
#'
#' @param gap_length The length of time in hours to consider a sampling gap.
#'   Default is \code{gap_length = 2} hours, which is twice the sampling
#'   interval of the least frequent sensor.
#'
#' @param gap_warning The length of time in hours to consider a substantial
#'   sampling gap. A warning can be printed if any gaps exceed this duration.
#'   Default is \code{gap_warning = 6} hours.
#'
#' @param quiet Logical argument to suppress Warning message when gap lengths
#'   exceed \code{gap_warning}. If \code{quiet = TRUE}, the Warning will not be
#'   printed.
#'
#' @return Returns summary table with \code{GAP_START},\code{GAP_LENGTH_DAYS},
#'   and \code{GAP_LENGTH_HOURS} for each grouping variable for all intervals
#'   between observations that exceed \code{gap_length}.
#'
#'   A warning will be displayed if any intervals exceed \code{gap_warning}.
#'
#' @importFrom dplyr group_by arrange mutate select lead right_join
#'
#' @export


check_for_data_gaps <- function(dat,
                                ...,
                                gap_length = 2,
                                gap_warning = 6,
                                quiet = FALSE){

  # to make sure all groups get assigned a value (even if no data gaps)
  out_table <- dat %>% distinct(..., DEPTH)


  gap_table <- dat %>%
    group_by(..., DEPTH) %>%
    # MUST be in chronological order
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    # difference between next observation and this observation in hours
    mutate(
      GAP_LENGTH_HOURS = as.numeric(
        difftime(lead(TIMESTAMP), TIMESTAMP, units = "hours")
      )
    ) %>%
    filter(GAP_LENGTH_HOURS > gap_length) %>%
    mutate(GAP_LENGTH_DAYS = round(GAP_LENGTH_HOURS / 24, digits = 2),
           GAP_LENGTH_HOURS = round(GAP_LENGTH_HOURS, digits = 2),
           GAP_START = TIMESTAMP) %>%
    select(..., DEPTH, GAP_START, GAP_LENGTH_HOURS, GAP_LENGTH_DAYS) %>%
    ungroup() %>%
    right_join(out_table) %>%
    mutate(
      GAP_LENGTH_HOURS = if_else(is.na(GAP_LENGTH_HOURS), 0, GAP_LENGTH_HOURS),
      GAP_LENGTH_DAYS = if_else(is.na(GAP_LENGTH_DAYS), 0, GAP_LENGTH_DAYS))

  if(any(gap_table$GAP_LENGTH_HOURS > gap_warning) & isFALSE(quiet)){

    n_warning <- nrow(filter(gap_table, GAP_LENGTH_HOURS > gap_warning))

    warning(n_warning, " substantial data gaps found")

  }

  gap_table

}










