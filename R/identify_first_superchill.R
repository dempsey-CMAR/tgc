#' Identify when VALUE first crosses below a threshold
#'
#' @details Identifies \code{TIMESTAMP} when \code{VALUE} first crosses below a
#'   threshold for each group in \code{DEPTH} and \code{...}.
#'
#'   If the \code{VALUE} does not cross the threshold for any groups, a
#'   data frame with 0 rows will be returned.
#'
#' @inheritParams identify_trending_up
#'
#' @param superchill_threshold The threshold for "superchill". Default is
#'   \code{superchill_threshold = -0.7}. The first observation below
#'   \code{superchill_threshold} triggers the end of the growing season for each
#'   group in \code{DEPTH} and \code{...}.
#'
#' @return Returns a tibble with the \code{TIMESTAMP} for one minute before the
#'   first time \code{VALUE} goes below \code{superchill_threshold} (for each
#'   \code{DEPTH} and group in \code{...}).
#'
#'   This \code{TIMESTAMP} is passed to \code{identify_growing_seasons()} to
#'   denote the end of the growing season.
#'
#'   No row will be returned for groups for which \code{VALUE} did not cross
#'   \code{superchill_threshold}.
#'
#' @importFrom dplyr arrange mutate filter summarise group_by ungroup left_join
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

  chill_groups <- dat %>%
    distinct(..., DEPTH)

  chill_table <- dat %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
   #mutate(YEAR = year(TIMESTAMP)) %>%
    group_by(..., DEPTH) %>%
    arrange(TIMESTAMP, .by_group = TRUE) %>%
    mutate(CROSS_THRESH = if_else(
      lag(VALUE) > superchill_threshold & VALUE <= superchill_threshold, TRUE,
      FALSE )
    ) %>%
    filter(CROSS_THRESH) %>%
    ungroup()
  # mutate(FIRST_CHILL = NA)

  if(nrow(chill_table) == 0){

    chill_table <- chill_groups %>%
      mutate(FIRST_CHILL = as_datetime(NA_character_))

  } else {

    # if(nrow(first_chill) > 0){
    chill_table <- chill_groups %>%
      left_join(chill_table) %>%
      group_by(..., DEPTH) %>%
      summarise(FIRST_CHILL = min(TIMESTAMP)) %>%
      # because the VALUE at min(TIMESTAMP) is < superchill_threshold
      # (don't want to include that value in filtered data)
      mutate(FIRST_CHILL = FIRST_CHILL - minutes(1)) %>%
      ungroup()
  }

  chill_table

}
