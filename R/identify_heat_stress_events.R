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
#' @importFrom data.table rleid
#'
#' @export


identify_heat_stress_events <- function(dat,
                                        ...,
                                        heat_threshold = 18,
                                        n_hours = 24){

  identify_heat_stress_intervals(dat = dat,
                                 ...,
                                 heat_threshold = heat_threshold,
                                 n_hours = n_hours) %>%
    dplyr::group_by(..., DEPTH) %>%
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
      # edge cases
      event_id3 = case_when(
        # if first interval does not overlap with the second interval
        interval_start == min(interval_start) & overlap == FALSE ~
          as.double(0),
        # for the last observation, overlap = NA, so event_id2 = NA
        # if last interval overlaps with second last interval, assign second last intervals id
        interval_start == max(interval_start) & dplyr::lag(overlap) == TRUE ~
          as.double(dplyr::lag(event_id2)),
        # if last interval does not overlap with second last interval, use original id
        interval_start == max(interval_start) & dplyr::lag(overlap) == FALSE ~
          as.double(event_id1),
        TRUE ~ as.double(event_id2)
      )
    ) %>%
    # find first last and last end of each event
    dplyr::group_by(..., DEPTH, event_id3) %>%
    dplyr::summarise(
      stress_start = min(interval_start),
      stress_end = max(interval_end),
    ) %>%
    dplyr::mutate(event_id = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(..., DEPTH, event_id, stress_start, stress_end)

}
