# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html

utils::globalVariables(c(

  # calculate_DO_concentration
  "cp",
  "SENSOR_NAME",

  # calculate_degree_days
  "n_DAYS",
  "AVG_TEMPERATURE",
  "START_DAY",
  "END_DAY",
  "PERIOD",
  "DATE",
  "VARIABLE",
  "TIMESTAMP",
  "VALUE",
  "DEPTH",

  # filter_days
  "EXCEED_THRESH",
  "n_obs",

  #identify_trending_up_days
  "CROSS_THRESH",
  "START_TREND",

  # calculate_event_duration
  "tmp",
  "event_id",
  "event_duration_days",

  # identify_heat_stress_intervals
  "EXCEED_THRESH",
  "interval_start",
  "interval_end",

  # identify_heat_stress_events
  "overlap",
  "event_id1",
  "event_id2",
  "event_id3",
  "stress_start",
  "stress_end"

))

# to add packages to DESCRIPTION:
#usethis::use_package("packageName")
