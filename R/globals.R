# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html

utils::globalVariables(c(

  # calculate_DO_concentration
  "cp",
  "SENSOR_NAME",

  # count_degree_days
  "AVG_TEMPERATURE",
  "VARIABLE",
  "TIMESTAMP",
  "VALUE",
  "DEPTH",
  "n_degree_days",

  # filter_days
  "EXCEED_THRESH",
  "n_obs",

  # identify_trending_up
  "CROSS_THRESH",
  "START_TREND",

  # identify_first_superchill
  "FIRST_CHILL",

  # calculate_event_duration
  "tmp",
  "event_id",
  "event_duration_days",

  # identify_heat_stress_intervals
  "EXCEED_THRESH",
  "interval_start",
  "interval_end",

  # identify_heat_stress_events
  "overlap_lag",
  "overlap_lead",
  "int_id",
  "event_id",

  "stress_start",
  "stress_end",

  # st_filter_heat_stress_events
  "STATION",

  # identify_growing_seasons
  "MONTH",
  "YEAR",
  "START_SEASON",
  "END_SEASON",
  "SEASON",
  "ID",
  "MAX_TIMESTAMP",
  "MIN_TIMESTAMP",
  "MAX_TIMESTAMP_NA",
  "MIN_TIMESTAMP_NA",

  # filter_in_growing_seasons
  "SEASON_DAYS",
  "SEASON_MONTHS",

  # count_growing_days
  "STOCKED_DAYS",
  "n_filtered_days",
  "n_growing_days",

  # count_max_event_days
  "max_event_days",

  # check_for_data_gaps
  "GAP_START",
  "GAP_LENGTH_HOURS",
  "GAP_LENGTH_DAYS",

  # tgc_functions
  "FINAL_WEIGHT",
  "TGC_FINAL_WEIGHT",
  "INITIAL_WEIGHT",
  "TGC_INITIAL_WEIGHT",
  "TGC",
  "TGC_DEGREE_DAYS",
  "INDEX",
  "CHECK"

))

# to add packages to DESCRIPTION:
#usethis::use_package("packageName")
