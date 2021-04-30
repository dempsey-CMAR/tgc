#' Identifies start and end of non-overlapping heat stress events
#'
#' @details Every observation that exceeds the threshold has a corresponding
#'   heat stress interval (\code{TIMESTAMP} of the observation + n_hours).
#'   Intervals may overlap with one or more other intervals.
#'
#'   Heat stress events are denoted by the beginning and end of overlapping
#'   intervals for each depth. Heat stress events do not overlap, but they may
#'   end and start on consecutive timestamps.
#'
#'
#' @inheritParams identify_heat_stress_intervals
#'
#' @return Returns a dataframe with four columns: \code{DEPTH}, \code{event_id},
#'   \code{stress_start} and \code{stress_end}.
#'
#'   Note: Events for a given depth do not overlap, but \code{event_end} and
#'   \code{event_start} may be consecutive timestamps.
#'
#' @importFrom dplyr mutate select group_by arrange summarise ungroup lead lag
#'   case_when
#' @importFrom data.table rleid
#'
#' @export


identify_heat_stress_events <- function(dat,
                                        threshold = 18,
                                        n_hours = 24){

  identify_heat_stress_intervals(dat = dat,
                                 threshold = threshold,
                                 n_hours = n_hours) %>%
    dplyr::group_by(DEPTH) %>%
    dplyr::arrange(interval_start, .by_group = TRUE) %>%
    # if this interval overlaps with the next interval, TRUE
    # FALSE indicates the LAST overlap
    dplyr::mutate(
      overlap = dplyr::lead(interval_start) <= interval_end,

      event_id1 = data.table::rleid(overlap),
      # don't combine these to a single case_when (id3 could be id1 or id2)

      # if current event overlaps with previous event (lag(overlap) == TRUE),
      # but not the next (overlap == FALSE), assign previous event id
      event_id2 = case_when(
        overlap == FALSE & dplyr::lag(overlap) == TRUE ~
          dplyr::lag(event_id1),
        TRUE ~ event_id1
      ),
      # for the last observation (overlap = NA, so event_id2 = NA)
      event_id3 = case_when(
        interval_start == max(interval_start) & dplyr::lag(overlap) == TRUE ~
          dplyr::lag(event_id2),
        interval_start == max(interval_start) & dplyr::lag(overlap) == FALSE ~
          event_id1,
        TRUE ~ event_id2
      )
    ) %>%
    # find first last and last end of each event
    dplyr::group_by(DEPTH, event_id3) %>%
    dplyr::summarise(
      stress_start = min(interval_start),
      stress_end = max(interval_end),
    ) %>%
    dplyr::mutate(event_id = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(DEPTH, event_id, stress_start, stress_end)

}
