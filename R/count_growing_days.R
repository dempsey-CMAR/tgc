#' Count number of days that were suitable for growth
#'
#' @details Days suitable for growth are days that remain after applying season
#'   and heat stress filters. Option to remove days with data gaps (otherwise
#'   the assumption is that the temperature on these days is the average
#'   temperature of the data series).
#'
#'   Results are automatically grouped by \code{SEASON} and \code{DEPTH}.
#'
#'   Runs filter_in_growing_seasons() if no SEASON column in the data. If there
#'   is not a full season of data, make sure to set argument full_season = FALSE
#'   or add a SEASON column to dat.
#'
#' @inheritParams apply_dd_filters
#'
#' @inheritParams identify_trending_up
#'
#' @inheritParams check_for_data_gaps
#'
#' @param ... Additional columns in \code{dat} to use as grouping variables.
#'   Results are automatically grouped by \code{SEASON} and \code{DEPTH}.
#'
#' @param rm_gap_days Logical argument indicating whether to remove days with
#'   data gaps when counting n_growing_days.
#'
#' @param gap_length The length of time in hours to consider a sampling gap.
#'   Default is gap_length = 2 hours, which is twice the sampling interval of
#'   the least frequent sensor. The total sample gap length (sum of all sample
#'   gaps) is subtracted from the number of stocked days to calculate the number
#'   of growing days.
#'
#' @return Returns a tibble with columns: \code{...}, \code{DEPTH},
#'   \code{SEASON}, \code{START_SEASON} (minimum TIMESTAMP for each group),
#'   \code{END_SEASON} (maximum TIMESTAMP for each group), \code{STOCKED_DAYS}
#'   (\code{difftime(END_SEASON, START_SEASON, units = "days")}),
#'   \code{n_filtered_days} (calculated from
#'   \code{identify_heat_stress_events}), and \code{n_growing_days}
#'   (STOCKED_DAYS - n_filtered_days).
#'
#' @importFrom dplyr mutate group_by summarize left_join if_else
#'
#' @export

count_growing_days <- function(dat,
                               ...,

                               heat_threshold = 18,
                               n_hours = 24,

                               trend_threshold = 4,
                               superchill_threshold = -0.7,
                               max_season = 540,
                               full_season = TRUE,

                               rm_gap_days = FALSE,
                               gap_length = 2,
                               gap_warning = 6,
                               quiet = TRUE
){

  # Define seasons if required  -------------------------------------------------------
  if(!("SEASON" %in% colnames(dat))){

    message(paste0("SEASON column not found.
                 \nApplying filter_in_growing_seasons() with full_season = ",
                   full_season))

    dat <- filter_in_growing_seasons(
      dat,
      trend_threshold = trend_threshold,
      superchill_threshold = superchill_threshold,
      max_season = max_season,
      full_season = full_season
    )

  }

  # number of days filtered out due to heat stress events for each SEASON, DEPTH, and ...
  # identify_heat_stress_events() automatically groups by DEPTH
  filtered_days <- identify_heat_stress_events(
    dat,
    ..., SEASON,
    heat_threshold = heat_threshold,
    n_hours = n_hours
  ) %>%
    mutate(
      n_filtered_days = as.numeric(
        difftime(stress_end, stress_start, units = "day")
      )
    ) %>%
    group_by(..., SEASON, DEPTH) %>%
    summarize(n_filtered_days = sum(n_filtered_days)) %>%
    ungroup()

  # join dat with number of filtered days
  dat_out <- dat %>%
    group_by(..., SEASON, DEPTH) %>%
    summarise(
      START_SEASON = min(TIMESTAMP),
      END_SEASON = max(TIMESTAMP)
    ) %>%
    mutate(
      STOCKED_DAYS = as.numeric(
        difftime(END_SEASON, START_SEASON, units = "days")
      )
    ) %>%
    left_join(filtered_days) %>%
    mutate(
      n_filtered_days = if_else(is.na(n_filtered_days), 0, n_filtered_days),
      n_growing_days = round(STOCKED_DAYS - n_filtered_days, digits = 2),

      STOCKED_DAYS = round(STOCKED_DAYS, digits = 2),
      n_filtered_days = round(n_filtered_days, digits = 2)
    ) %>%
    ungroup()


  if(isTRUE(rm_gap_days)){

    # number of days without data
    gap_days <- dat %>%
      check_for_data_gaps(
        ..., SEASON,
        gap_length = gap_length,
        gap_warning = gap_warning,
        quiet = quiet
      ) %>%
      group_by(..., SEASON, DEPTH) %>%
      summarize(n_gap_days = sum(GAP_LENGTH_DAYS)) %>%
      ungroup()

    # subtract number of heat stress days and missing days from total number of days in season
    dat_out <- dat_out %>%
      left_join(gap_days) %>%
      mutate(
        n_growing_days = round(n_growing_days - n_gap_days, digits = 2)
      ) %>%
      select(..., SEASON, DEPTH, START_SEASON, END_SEASON,
             STOCKED_DAYS, n_filtered_days, n_gap_days, n_growing_days)

  }

  dat_out

}
