#' Filters out data from days when the value exceeds a threshold
#'
#' @description Filters out all data from days when the value exceeds a given
#'   threshold for a given number of observations. Option to filter out
#'   additional days after the threshold is exceeded.
#'
#'   \code{dat.tidy } should only include data for one variable.
#'
#' @inheritParams identify_days_to_filter
#' @return Returns dat, filtered when triggered by \code{threshold} and
#'   \code{min.exceedence}.
#'
#' @importFrom dplyr select anti_join
#' @importFrom lubridate as_date days
#'
#' @export

filter_days <- function(dat,
                        threshold = 20,
                        min_exceedance = 1,
                        n_extra_days = 0) {


  # if("VARIABLE" %in% colnames(dat.tidy)){
  #
  #   if(length(unique(dat.tidy$VARIABLE)) > 1) {
  #
  #     stop("More than one VARIABLE found in dat.tidy. \n
  #        HINT: filter dat.tidy for the variable of interest before applying
  #        filter_days()")
  #   }
  # }
  #
  # dat.tidy <- dat.tidy %>%
  #   mutate(DATE = lubridate::as_date(TIMESTAMP))
  #
  # # table of DEPTH and the DATE for which the threshold is exceeded
  # to_remove1 <- dat.tidy %>%
  #   mutate(EXCEED_THRESH = if_else(VALUE >= threshold, TRUE, FALSE)) %>%
  #   group_by(DEPTH, DATE) %>%
  #   summarise(n_obs = sum(EXCEED_THRESH)) %>%
  #   filter(n_obs >= min.exceedance) %>%
  #   select(-n_obs)
  #
  # # copy the rows of the table and add n.extra days to each date
  # to_remove <- to_remove1 %>%
  #   rbind(
  #     to_remove1 %>% mutate(DATE = DATE + lubridate::days(n.extra.days))
  #   ) %>%
  #   distinct(DATE, DEPTH)

  to_remove <- identify_days_to_filter(
    dat = dat,
    threshold = threshold,
    min_exceedance = min_exceedance,
    n_extra_days
  )

  # anti-join to remove the rows in to_remove from dat.tidy
  dat %>%
    anti_join(to_remove, by = c("DEPTH", "DATE")) %>%
    select(-DATE)

}
