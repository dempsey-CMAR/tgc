

# not sure if I need the window... or if YEAR is good enough (OR
# group by DEPTH and YEAR????)

find_trending_up_date <- function(dat, year){


dat_trend_up <- data_dd %>%
  filter(TIMESTAMP >= values$zoom.start, TIMESTAMP <= values$zoom.end) %>%
  group_by(DEPTH) %>%
  mutate(CROSS_THRESH = if_else(lag(VALUE) < lower_thresh & VALUE >= lower_thresh, TRUE, FALSE )) %>%
  filter(CROSS_THRESH == TRUE) %>%
  summarise(START_TREND = max(TIMESTAMP)) %>%
  ungroup()

# left join in case there is a DEPTH that is NOT measured within the window of zoom.start
# and zoom.end (i.e., that DEPTH will not be in the table dat_trend_up, and therefore not
# included in dat_dd_plot)
dat_dd_plot <- left_join(data_dd, dat_trend_up, by = "DEPTH") %>%
  group_by(DEPTH) %>%
  mutate(START_TREND = if_else(is.na(START_TREND), min(TIMESTAMP), START_TREND)) %>%
  mutate(TREND_UP = if_else(TIMESTAMP >= START_TREND, TRUE, FALSE)) %>%
  mutate(DEPTH = if_else(TREND_UP == FALSE, "0", as.character(DEPTH))) %>%
  ungroup() %>%
  convert_depth_to_ordered_factor()

}
