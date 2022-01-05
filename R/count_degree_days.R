#' @title Count number of degree-days
#'
#' @details Degree-days = average temperature over \emph{n} days * \emph{n} days
#'
#'   \emph{n} is the number of days suitable for growth (from
#'   \code{count_growing_days()}).
#'
#'   Results are grouped by \code{SEASON}, \code{DEPTH}, and \code{...}.
#'
#' @inheritParams count_growing_days
#'

#'
#' @return Returns a tibble with columns:

#' @family calculate

#' @author Danielle Dempsey

#' @importFrom dplyr summarise group_by left_join ungroup select %>%
#' @importFrom lubridate date parse_date_time

#' @export

count_degree_days <- function(dat,
                              ...,
                              heat_threshold = 18,
                              n_hours = 24,
                              trend_threshold = 4,
                              superchill_threshold = -0.7,
                              max_season = 540,
                              full_season = TRUE){


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

  # browser()

  # Count growing days in each group (SEASON, DEPTH, ...) -------------------

  growing_days <- count_growing_days(
    dat,
    ...,                              # automatically grouped by SEASON and DEPTH
    #apply_season_filt = FALSE,        # season defined above
    heat_threshold = heat_threshold,
    n_hours = n_hours
  )


  # Count degree-days for each group  -------------------------------------------------------

  dat %>%
    filter_out_heat_stress_events(     # automatically grouped by STATION and/or SEASON and DEPTH
      heat_threshold = heat_threshold,
      n_hours = n_hours
    ) %>%
    group_by(..., SEASON, DEPTH) %>%
    summarise(
      # number of observations in each group
      n_OBSERVATIONS = n(),
      # average temperature in group
      AVG_TEMPERATURE =  round(mean(VALUE), digits = 3)
    ) %>%
    left_join(growing_days) %>%
    mutate(
      n_degree_days = round(n_growing_days * AVG_TEMPERATURE, digits = 2)
    ) %>%
    ungroup() %>%
    select(..., SEASON, DEPTH,
           START_SEASON, END_SEASON, STOCKED_DAYS,
           n_filtered_days, n_growing_days, AVG_TEMPERATURE, n_degree_days)

}









