#' Count the maximum number of days VALUE exceeds a threshold (above or below)
#'
#' @param dat Dataframe that must include columns VALUE (numeric) and TIMESTAMP
#'   (POSIXct), and a column for each grouping variable in \code{...}.
#'
#' @param threshold Value to cross to trigger exceedance (exclusive)
#'
#' @param ... Names of columns that should be used as grouping variables (not
#'   quoted), e.g. \code{DEPTH, YEAR}.
#'
#' @param less_than Logical argument. If \code{TRUE} (the default), function
#'   will identify events when VALUE was BELOW \code{threshold}; if
#'   \code{FALSE}, will identify events when VALUE was ABOVE the
#'   \code{threshold}.
#'
#' @param summary_table Logical argument. If \code{TRUE} (the default), will
#'   return summary table with maximum event duration for each grouping
#'   variable. If \code{FALSE}, will return table with all exceedance events for
#'   each grouping variable.
#'
#' @return Returns summary table with maximum event duration for each grouping
#'   variable OR a table with all exceedance events for each grouping variable.
#'
#' @importFrom expss less greater
#' @importFrom dplyr case_when distinct group_by ungroup arrange mutate filter
#'   summarise
#'
#' @export


count_max_event_days <- function(dat,
                                 threshold,
                                 ...,
                                 less_than = TRUE,
                                 summary_table = TRUE){

  if(isTRUE(less_than)) exceed_thresh <- expss::less(threshold)

  if(isFALSE(less_than)) exceed_thresh <- expss::greater(threshold)

  # to make sure all groups get assigned a count (even if threshold is never crossed)
  out_table <- dat %>% distinct(...)

  dat_out <- dat %>%
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
      max_event_days = difftime(max(TIMESTAMP), min(TIMESTAMP), units = "days")
    ) %>%
    mutate(
      max_event_days = round(unclass(max_event_days), digits = 2)
    ) %>%
    ungroup() %>%
    right_join(out_table) %>%
    mutate(
      max_event_days = case_when(is.na(start_day) & is.na(end_day) ~ 0,
                                 TRUE ~ max_event_days)
    )

  if(isTRUE(summary_table)){

    dat_out <- dat_out %>%
      group_by(...) %>%
      summarise(MAX_EVENT_DAYS = max(max_event_days)) %>%
      ungroup()
  }

  dat_out

}
