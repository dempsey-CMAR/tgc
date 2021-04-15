

identify_first_superchill_days <- function(dat, lower_threshold = -0.7, ...){


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
      lag(VALUE) < lower_threshold & VALUE >= lower_threshold, TRUE, FALSE )
    ) %>%
    filter(CROSS_THRESH) %>%
    summarise(START_TREND = min(TIMESTAMP)) %>%
    ungroup()

}
