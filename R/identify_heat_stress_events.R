#' Identify start and end of non-overlapping heat stress events
#'
#' @details Every observation that exceeds the threshold is assigned an heat
#'   stress interval (\code{TIMESTAMP} of the observation + n_hours). Intervals
#'   may overlap with one or more other intervals.
#'
#'   Heat stress events are denoted by the beginning and end of overlapping
#'   intervals for each \code{DEPTH} and group in \code{...}. Heat stress events
#'   do not overlap, but they may end and start on consecutive
#'   \code{TIMESTAMP}s.
#'
#' @inheritParams identify_trending_up
#'
#' @inheritParams identify_heat_stress_intervals
#'
#' @return Returns a tibble with columns: \code{...}, \code{DEPTH},
#'   \code{event_id}, \code{stress_start} and \code{stress_end}.
#'
#'   Note: Events for a given depth do not overlap, but \code{event_end} and
#'   \code{event_start} may be consecutive timestamps.
#'
#' @importFrom dplyr mutate select group_by arrange summarise ungroup lead lag
#'   case_when
#' @importFrom purrr map_df
#'
#' @export


identify_heat_stress_events <- function(dat,
                                        ...,
                                        heat_threshold = 18,
                                        n_hours = 24){

  ints <- identify_heat_stress_intervals(dat = dat,
                                           ...,
                                           heat_threshold = heat_threshold,
                                           n_hours = n_hours) %>%
    dplyr::group_by(..., DEPTH) %>%
    dplyr::arrange(interval_start, .by_group = TRUE) %>%
    dplyr::mutate(

      # overlap with previous interval?
      overlap_lag = interval_start <= dplyr::lag(interval_end),
      # first observation cannot over lap with previous
      overlap_lag = if_else(
        interval_start == min(interval_start), FALSE, overlap_lag
      ),

      # overlap with next interval?
      overlap_lead = dplyr::lead(interval_start) <= interval_end,
      # last observation cannot over lap with next
      overlap_lead = if_else(
        interval_start == max(interval_start), FALSE, overlap_lead
      ),

      int_id = case_when(

        overlap_lag == FALSE & overlap_lead == FALSE ~ 99, # single obs event
        overlap_lag == FALSE & overlap_lead == TRUE ~ 1,   # beginning of event
        overlap_lag == TRUE & overlap_lead == TRUE ~ 2,    # middle of event
        overlap_lag == TRUE & overlap_lead == FALSE ~ 3    # end of event

      ),

      int_id_lead = lead(int_id)
    ) %>%
    ungroup()

  events_out <- list()

  k <- 1 # id counter

  for(i in seq_along(1:nrow(ints))){

    int.i <- ints[i, ]

    # stop with error if any id combos that should not exist
    # e.g., can't have 99 (no overlaps) followed by 2 (overlap with previous and next interval)
    if(!is.na(int.i$int_id_lead)){
      with(
        int.i,
        if(int_id == 99 && int_id_lead == 2 |
           int_id == 99 && int_id_lead == 3 |
           int_id == 1 && int_id_lead == 1 |
           int_id == 1 && int_id_lead == 99 |
           int_id == 2 && int_id_lead == 1 |
           int_id == 2 && int_id_lead == 99 |
           int_id == 3 && int_id_lead == 2 |
           int_id == 3 && int_id_lead == 3 ){

          stop("something weird happened with assigning heat stress event int_ids")

        }
      )
    }

    # assign event id
    int.i$event_id <- k

    events_out[[i]] <- int.i

    # update id at the end of event
    if(int.i$int_id == 99 | int.i$int_id == 3) k <- k + 1

    # reset id at the end of the group
    if(is.na(int.i$int_id_lead)) k <- 1

  }

events_out %>%
    map_df(rbind) %>%
    # find first and last timestamp of each event
    dplyr::group_by(..., DEPTH, event_id) %>%
    dplyr::summarise(
      stress_start = min(interval_start),
      stress_end = max(interval_end),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(..., DEPTH, event_id, stress_start, stress_end)

}
