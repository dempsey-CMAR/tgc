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
#' @param growing_days Optional dataframe including columns \code{DEPTH},
#'   \code{SEASON}, \code{...} (if required) and {n_growing_days}. If not
#'   provided, it will be calculated using the \code{count_growing_days}
#'   function.
#'
#' @return Returns a tibble with columns:

#' @family calculate

#' @author Danielle Dempsey

#' @importFrom dplyr summarise group_by left_join ungroup select %>% everything
#'   relocate
#' @importFrom lubridate date parse_date_time

#' @export

count_degree_days <- function(dat,
                              ...,
                              growing_days = NULL,

                              heat_threshold = 18,
                              n_hours = 24,

                              trend_threshold = 4,
                              superchill_threshold = -0.7,
                              max_season = 540,
                              full_season = TRUE,

                              rm_gap_days = FALSE,
                              gap_length = 2,
                              gap_warning = 6,
                              quiet = TRUE){


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

  # Count growing days in each group (SEASON, DEPTH, ...) -------------------
  if(is.null(growing_days)){

     growing_days <- count_growing_days(
      dat,
      ...,                              # automatically grouped by SEASON and DEPTH
      heat_threshold = heat_threshold,
      n_hours = n_hours,

      rm_gap_days = rm_gap_days,
      gap_length = gap_length,
      gap_warning = gap_warning,
      quiet = quiet
    )

  }

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
           START_SEASON, END_SEASON,
           STOCKED_DAYS, n_filtered_days, everything()) %>%
    relocate(AVG_TEMPERATURE, .before = n_degree_days)
           #n_gap_days, n_growing_days,
           #AVG_TEMPERATURE, n_degree_days)

}









