



# less_than: if true, function will identify events when VALUE was BELOW
# threshold; if FALSE, will identify events when VALUE was ABOVE the threshold

# summary_table: if FALSE, will return table with all exceedance events for each
# grouping variable. If TRUE, will return summary table with maximum event
# duration for each grouping variable

# Notes:
# no row will be returned for level of grouping variable for which there
# were no exceedance events. e.g., if there was no superchill at 5 m, there will
# be no row for 5 m in dat.out

# Do not need to arrange by variables in ... (grouping takes care of that)

#' Calculate the duration of time VALUE exceeds a threshold (above or below)
#'
#' @param dat Data frame with columns \code{VALUE} (numeric), \code{TIMESTAMP}
#'   (POSIXct), and a column for each grouping variable in \code{...}.
#'
#' @param threshold Threshold value that triggers start of event. Event starts
#'   when \code{VALUE} is greater than (less than) OR equal to \code{threshold}.
#'
#' @param ... Grouping variables. Must be name(s) of column(s) in \code{dat}.
#'
#' @param less_than Logical argument. If \code{TRUE}, function identifies events
#'   when \code{VALUE} was less than or equal to \code{threshold}; if
#'   \code{FALSE}, the function identifies events when \code{VALUE} was greater
#'   than or equal to \code{threshold}.
#'
#' @param summary_table Logical argument. If \code{FALSE}, function will return
#'   table with all events for each grouping variable. If \code{TRUE}, function
#'   will return summary table with maximum event duration for each grouping
#'   variable.
#'
#' @return Returns a data frame.
#'
#' @importFrom data.table rleid
#' @importFrom dplyr arrange filter group_by  mutate summarise ungroup
#' @importFrom expss ge le
#'
#' @export

calculate_event_duration <- function(
    dat,
    threshold,
    ...,
    less_than = TRUE,
    summary_table = TRUE){

  if(isTRUE(less_than)) exceed_thresh <- expss::le(threshold) # less than or equal to

  if(isFALSE(less_than)) exceed_thresh <- expss::ge(threshold) # greater than or equal to

  dat_out <- dat %>%
    group_by(...) %>%
    # MUST be in chronological order within groups for rleid to give correct result
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    # add id for different events
    mutate(tmp = exceed_thresh(VALUE),
           event_id = data.table::rleid(tmp)) %>%
    # keep only periods that exceed the threshold
    filter(tmp) %>%
    # for each event, get its duration
    group_by(..., event_id) %>%
    summarise(
      start_day = min(TIMESTAMP),
      end_day = max(TIMESTAMP),
      event_duration_days = difftime(max(TIMESTAMP), min(TIMESTAMP), units = "days")
    ) %>%
    mutate(
      event_duration_days = round(unclass(event_duration_days), digits = 2)
    ) %>%
    ungroup()

  if(isTRUE(summary_table)){

    dat_out <- dat_out %>%
      group_by(...) %>%
      summarise(MAX_DURATION_DAYS = max(event_duration_days)) %>%
      ungroup()
  }

  dat_out

}
