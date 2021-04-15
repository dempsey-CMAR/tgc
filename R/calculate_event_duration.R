#' Calculate the duration of time VALUE exceeds a threshold (above or below)
#'
#' @details No row will be returned for level of grouping variable for which
#'   there were no exceedance events. e.g., if there was no superchill at 5 m,
#'   there will be no row for 5 m in dat.out.
#'
#'   Do not need to arrange by variables in ... (grouping takes care of that).

#' @param dat Dataframe that must include columns VALUE (numeric) and TIMESTAMP
#'   (POSIXct), and a column for each grouping variable in \code{...}.
#' @param threshold Value to cross to trigger exceedance (exclusive)
#' @param ... Names of columns that should be used as grouping variables (not
#'   quoted), e.g. \code{DEPTH, YEAR}.
#' @param less_than Logical argument. If \code{TRUE} (the default), function
#'   will identify events when VALUE was BELOW \code{threshold}; if
#'   \code{FALSE}, will identify events when VALUE was ABOVE the
#'   \code{threshold}.
#' @param summary_table Logical argument. If \code{TRUE} (the default), will
#'   return summary table with maximum event duration for each grouping
#'   variable. If \code{FALSE}, will return table with all exceedance events for
#'   each grouping variable.
#'
#' @return Returns summary table with maximum event duration for each grouping
#'   variable OR a table with all exceedance events for each grouping variable.
#'
#' @importFrom expss less greater
#'
#' @export


calculate_event_duration <- function(dat,
                                     threshold,
                                     ...,
                                     less_than = TRUE,
                                     summary_table = TRUE){

  if(isTRUE(less_than)) exceed_thresh <- expss::less(threshold)

  if(isFALSE(less_than)) exceed_thresh <- expss::greater(threshold)

  dat.out <- dat %>%
    group_by(...) %>%
    # MUST be in chronological order within groups for rleid to give correct result
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    # add id for different events
    mutate(
      tmp = exceed_thresh(VALUE),
      event_id = data.table::rleid(tmp)
    ) %>%
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

    dat.out <- dat.out %>%
      group_by(...) %>%
      summarise(MAX_DURATION_DAYS = max(event_duration_days)) %>%
      ungroup()
  }

  dat.out

}
