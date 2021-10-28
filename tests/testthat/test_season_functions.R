# NOTE: only S1, DEPTH = 2 m will have an END_SEASON
## there is no superchill for S1, DEPTH = 5 AND
## the function is looking for superchill **in the next year** for S2

library(tgc)
library(dplyr)
library(lubridate)

# Set up ------------------------------------------------------------------

source(system.file("testdata/test_data.R", package = "tgc"))

# plot_temperature_at_depth(dat, facet_var = "STATION")

# find first value for each season (first obs is BEFORE the beginning of the season)
# note that for S1, DEPTH = 2, START_TREND should be NA because there are
# no observations < 4
check_trend <- dat %>%
  group_by(STATION, SEASON, DEPTH) %>%
  mutate(INDEX = 1:n()) %>%
  filter(INDEX != 1) %>%
  select(-INDEX) %>%
  summarise(START_TREND = min(TIMESTAMP)) %>%
  mutate(START_TREND = if_else(SEASON == "S1" & DEPTH == 2,
                               as_datetime(NA_character_), START_TREND))

dat <- select(dat, -SEASON)

#  identify_trending_up() --------------------------------------------------

trend_up <- identify_trending_up(dat, STATION)

trend_st1 <- filter(trend_up, STATION == "Station1")
trend_st2 <- filter(trend_up, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("identify_trending_up() worked for both stations",{

  expect_equal(select(trend_st1, -STATION), select(trend_st2, -STATION))

})

test_that("identify_trending_up() output expected results",{

  expect_equal(trend_st1$START_TREND, trend_st1$START_TREND)

})

#  identify_first_superchill() --------------------------------------------------

# first superchill will be at max(dat$TIMESTAMP) + minutes (15) for DEPTH = 2
# no superchill for DEPTH = 5
dat_chill <- tibble(TIMESTAMP = max(dat$TIMESTAMP) + minutes(c(15, 30, 45)),
                   STATION = "Station1", DEPTH = 2, VALUE = -0.7) %>%
  rbind(
    tibble(TIMESTAMP = max(dat$TIMESTAMP) + minutes(c(15, 30, 45)),
           STATION = "Station2", DEPTH = 2, VALUE = -0.7)
  ) %>%
  rbind(dat)

# plot_temperature_at_depth(dat_chill, facet_var = "STATION")

# FIRST_CHILL is 1 minute before first observation of superchill
chill <- identify_first_superchill(dat_chill, STATION) %>%
  mutate(chill_compare = FIRST_CHILL + minutes(1))

chill_st1 <- filter(chill, STATION == "Station1")
chill_st2 <- filter(chill, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("identify_first_superchill() worked for both stations",{

  expect_equal(select(chill_st1, -STATION), select(chill_st2, -STATION))

})


chill_match <- c(max(dat$TIMESTAMP) + minutes(15), NA)

test_that("identify_first_superchill() outputs expected results",{

  expect_equal(chill_st1$chill_compare, chill_match)

})

# identify_growing_seasons ------------------------------------------------

# NOTE: only S1, DEPTH = 2 m will have an END_SEASON
## there is no superchill for S1, DEPTH = 5 AND
## the function is looking for superchill **in the next year** for S2
seasons <- identify_growing_seasons(dat_chill, STATION)

seasons_st1 <- filter(seasons, STATION == "Station1")
seasons_st2 <- filter(seasons, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("identify_growing_seasons() worked for both stations",{

  expect_equal(select(seasons_st1, -STATION), select(seasons_st2, -STATION))

})

test_that("identify_growing_seasons() returns expected results", {

  expect_equal(seasons_st1$START_SEASON,
               filter(check_trend, STATION == "Station1")$START_TREND)
  expect_equal(as.character(seasons_st1$END_SEASON),
               c("2022-02-28 00:14:00", NA_character_, NA_character_, NA_character_))

})


partial_seasons <- identify_growing_seasons(dat_chill, STATION, full_season = FALSE)

p_seasons_st1 <- filter(partial_seasons, STATION == "Station1")
p_seasons_st2 <- filter(partial_seasons, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("identify_growing_seasons(dat, full_season = FALSE) worked for both stations",{

  expect_equal(select(p_seasons_st1, -STATION), select(p_seasons_st2, -STATION))

})

# check that function output correct results (slightly different logic for each case)
test_that("identify_growing_seasons(dat, full_season = FALSE) returns expected results",{

  expect_equal(as.character(p_seasons_st1$START_SEASON),
               c("2021-06-30 23:45:00",    # first obs = 4
                 "2021-07-01 00:00:00",    # first obs < 4 & second obs > 4
                 "2022-02-01 00:00:00",
                 "2022-02-01 00:00:00"))

  expect_equal(as.character(p_seasons_st1$END_SEASON),
               c("2022-02-28 00:14:00",    # first superchill
                 "2022-02-28 00:00:00",    # last obs bc no superchill
                 "2022-02-28 00:45:00",    # last obs bc looking for superchill in 2023
                 "2022-02-28 00:00:00"))   # last obs bc looking for superchill in 2023

})


# filter_in_growing_seasons() ---------------------------------------------
dat_filt <- filter_in_growing_seasons(dat_chill, full_season = FALSE)

filt <- dat_filt %>%
  group_by(STATION, SEASON, DEPTH) %>%
  summarise(START = min(TIMESTAMP), END = max(TIMESTAMP)) %>%
  ungroup()

filt_st1 <- filter(filt, STATION == "Station1")
filt_st2 <- filter(filt, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("filter_in_growing_seasons(dat, full_season = FALSE) worked for both stations",{

  expect_equal(select(filt_st1, -STATION), select(filt_st2, -STATION))

})


# check that function output correct results (slightly different logic for each case)
test_that("identify_growing_seasons(dat, full_season = FALSE) returns expected results",{

  expect_equal(as.character(filt_st1$START),
               c("2021-06-30 23:45:00",    # first obs = 4
                 "2021-07-01 00:00:00",    # first obs < 4 & second obs > 4
                 "2022-02-01 00:00:00",
                 "2022-02-01 00:00:00"))

  expect_equal(as.character(filt_st1$END),
               c("2022-02-28 00:00:00",    # superchill obs was filtered out
                 "2022-02-28 00:00:00",    # last obs bc no superchill
                 "2022-02-28 00:45:00",    # last obs bc looking for superchill in 2023
                 "2022-02-28 00:00:00"))   # last obs bc looking for superchill in 2023

})





# plot_temperature_at_depth(dat_filt, facet_var = "STATION + SEASON")
