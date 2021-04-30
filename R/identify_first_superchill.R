#' Identifies when VALUE first crosses below a lower threshold

#' @details Identifies TIMESTAMP when VALUE first crosses below threshold for
#'   each group in \code{...}.
#'
#' @inheritParams calculate_degree_days
#' @param superchill_threshold Default is \code{superchill_threshold = -0.7}.
#'   The first observation below \code{superchill_threshold} triggers the end of
#'   the growing season for each group in \code{...}.
#'
#' @return Returns the TIMESTAMP (for each group in \code{...}) for the first
#'   time VALUE goes below \code{superchill_threshold}.
#'
#'   This TIMESTAMP is passed to \code{identify_growing_seasons()} to denote the
#'   end of the growing season.
#'
#'   No row will be returned for groups for which VALUE did not cross
#'   \code{superchill_threshold}.
#'
#' @importFrom dplyr arrange mutate filter summarise ungroup
#' @importFrom lubridate year
#' @export


identify_first_superchill <- function(dat, ..., superchill_threshold = -0.7){

  if("VARIABLE" %in% colnames(dat)){

    if(length(unique(dat$VARIABLE)) > 1) {

      stop("More than one VARIABLE found in dat. \n
         HINT: filter dat for the variable of interest before applying
         function")
    }
  }

  dat %>%
    mutate(YEAR = year(TIMESTAMP)) %>%
    group_by(...) %>%
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    mutate(CROSS_THRESH = if_else(
      lag(VALUE) > superchill_threshold & VALUE <= superchill_threshold, TRUE,
      FALSE )
    ) %>%
    filter(CROSS_THRESH) %>%
    summarise(FIRST_CHILL = min(TIMESTAMP)) %>%
    # because the VALUE at min(TIMESTAMP) is < superchill_threshold
    # (don't want to include that value in filtered data)
    mutate(FIRST_CHILL = FIRST_CHILL - minutes(1)) %>%
    ungroup()

}
