# October 29, 2021

# test data has 2 stations, 2 seasons, 2 depths
## data for Station1 is the same as for Station2, so the first test in each
## section verifies that the results are the same for each station.
## The remaining tests are run for Station1

# To trigger a new season:
## lag(VALUE) < trend_threshold & VALUE >= trend_threshold (default trend_threshold = 4)
## For test data:
### the first observation for S1, DEPTH = 2 is 4 deg C, which DOES NOT trigger a season start
### the first observation for S1, DEPTH = 5 is 3 deg C, which DOES trigger season start

# To trigger end of season:
## lag(VALUE) > superchill_threshold & VALUE <= superchill_threshold (default superchill_threshold = -0.7)
## For test data:
## S1, DEPTH = 2: obs of -0.7 deg C were added at the end of February 2022, so
## season end IS triggered.
## S1, DEPTH = 5, No superchill obs were added, so season end is NOT triggered.
## For S2: the function is looking for superchill **in the next calendar year**
## (no observation) so there will be no season end for either depth for S2

# Set up ------------------------------------------------------------------

library(tgc)
library(dplyr)
library(lubridate)

source(system.file("testdata/test_data.R", package = "tgc"))

# plot_temperature_at_depth(dat, facet_var = "STATION")

# find first value for each season (first obs is BEFORE the beginning of the season)
# NOTE: for S1, DEPTH = 2, START_TREND should be NA because there are
# no observations < 4
check_trend <- dat %>%
  group_by(STATION, SEASON, DEPTH) %>%
  mutate(INDEX = 1:n()) %>%
  filter(INDEX != 1) %>%
  select(-INDEX) %>%
  summarise(START_TREND = min(TIMESTAMP)) %>%
  mutate(START_TREND = if_else(SEASON == "S1" & DEPTH == 2,
                               as_datetime(NA_character_), START_TREND))

# remove SEASON column so it will be assigned by functions
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

rm(trend_up, trend_st1, trend_st2)

#  identify_first_superchill() --------------------------------------------------

# add superchill observations
# for DEPTH = 2: season end will be at max(dat$TIMESTAMP) + minutes (15)
# for DEPTH = 5: no superchill so season end will be NA
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

# first observation of superchill
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

  # expect_equal(as.character(p_seasons_st1$START_SEASON),
  #              c("2021-06-30 23:45:00",    # first obs = 4
  #                "2021-07-01 00:00:00",    # first obs < 4 & second obs > 4
  #                "2022-02-01 00:00:00",    # first obs < 4 & second obs > 4
  #                "2022-02-01 00:00:00"))   # first obs < 4 & second obs > 4
  #
  # expect_equal(as.character(p_seasons_st1$END_SEASON),
  #              c("2022-02-28 00:14:00",    # first superchill
  #                "2022-02-28 00:00:00",    # last obs bc no superchill
  #                "2022-02-28 00:45:00",    # last obs bc looking for superchill in 2023
  #                "2022-02-28 00:00:00"))   # last obs bc looking for superchill in 2023

  expect_equal(
    p_seasons_st1$START_SEASON,
    as_datetime(
      c("2021-06-30 23:45:00",    # first obs = 4
        "2021-07-01 00:00:00",    # first obs < 4 & second obs > 4
        "2022-02-01 00:00:00",    # first obs < 4 & second obs > 4
        "2022-02-01 00:00:00")    # first obs < 4 & second obs > 4
    )
  )

  expect_equal(
    p_seasons_st1$END_SEASON,
    as_datetime(
      c("2022-02-28 00:14:00",    # first superchill
        "2022-02-28 00:00:00",    # last obs bc no superchill
        "2022-02-28 00:45:00",    # last obs bc looking for superchill in 2023
        "2022-02-28 00:00:00")    # last obs bc looking for superchill in 2023
    )
  )

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
#
#   expect_equal(as.character(filt_st1$START),
#                c("2021-06-30 23:45:00",    # first obs = 4
#                  "2021-07-01 00:00:00",    # first obs < 4 & second obs > 4
#                  "2022-02-01 00:00:00",
#                  "2022-02-01 00:00:00"))
#
#   expect_equal(as.character(filt_st1$END),
#                c("2022-02-28 00:00:00",    # superchill obs was filtered out
#                  "2022-02-28 00:00:00",    # last obs bc no superchill
#                  "2022-02-28 00:45:00",    # last obs bc looking for superchill in 2023
#                  "2022-02-28 00:00:00"))   # last obs bc looking for superchill in 2023


  expect_equal(
    filt_st1$START,
    as_datetime(
      c("2021-06-30 23:45:00",    # first obs = 4
        "2021-07-01 00:00:00",    # first obs < 4 & second obs > 4
        "2022-02-01 00:00:00",
        "2022-02-01 00:00:00")
    )
  )

  expect_equal(
    filt_st1$END,
    as_datetime(
      c("2022-02-28 00:00:00",  # superchill obs was filtered out
        "2022-02-28 00:00:00",    # last obs bc no superchill
        "2022-02-28 00:45:00",    # last obs bc looking for superchill in 2023
        "2022-02-28 00:00:00")    # last obs bc looking for superchill in 2023
    )
  )

})

