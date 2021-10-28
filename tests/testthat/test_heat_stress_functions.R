# October 21, 2021
# Test heat stress functions

# test data has 2 stations, 2 seasons, 2 depths
## data for Station1 is the same as for Station2, so the first test in each
## section verifies that the results are the same for each station.
## The remaining tests are run for Station1

# Note that VALUE = threshold (18) is flagged by identify_heat_stress_intervals()
## and subsequently filtered out by filter_out_heat_stress_events()
## example: Station1, DEPTH = 2 has 9 instances of VALUE = 18 that are filtered

library(lubridate)
library(data.table)
library(dplyr)
library(tgc)

source(system.file("testdata/test_data.R", package = "tgc"))

# shuffle to make sure order does not matter
set.seed(468)
rows <- sample(nrow(dat))
dat <- dat[rows, ]

#plot_temperature_at_depth(dat, facet_var = "STATION + DEPTH")


# identify_heat_stress_interval -------------------------------------------

heat_stress_int <- identify_heat_stress_intervals(dat, STATION, SEASON)  %>%
  mutate(DIFF = difftime(interval_end, interval_start, unit = "hour"))

# separate results by station
int_st1 <- filter(heat_stress_int, STATION == "Station1")
int_st2 <- filter(heat_stress_int, STATION == "Station2")

# check that function retuned same result for both STATIONS
test_that("identify_heat_stress_intervals() worked for both stations",{

  expect_equal(select(int_st1, -STATION), select(int_st2, -STATION))

})

# separate results for Station1 by SEASON and DEPTH
check_int1 <- filter(int_st1, SEASON == "S1", DEPTH == 2)
check_int2 <- filter(int_st1, SEASON == "S1", DEPTH == 5)

check_int3 <- filter(int_st1, SEASON == "S2", DEPTH == 2)
check_int4 <- filter(int_st1, SEASON == "S2", DEPTH == 5)


# check that all TIMESTAMPs identified by function are in exceed &
# that the function identified all TIMESTAMPs from exceed
test_that("identify_heat_stress_intervals() finds correct interval_start",{

  expect_equal(check_int1$interval_start, sort(exceed1))
  expect_equal(check_int2$interval_start, sort(exceed2))
  expect_equal(check_int3$interval_start, sort(exceed3))
  expect_equal(check_int4$interval_start, sort(exceed4))

})

# check that all heat stress intervals are 24 hours
test_that("identify_heat_stress_intervals() finds correct interval_end",{

  expect_equal(unique(heat_stress_int$DIFF), 24)

})


# identify_heat_stress_events ---------------------------------------------

heat_stress_events <- dat %>%
  identify_heat_stress_events(STATION, SEASON) %>%
  mutate(DIFF = difftime(stress_end, stress_start, unit = "hour"))

# separate results by station
events_st1 <- filter(heat_stress_events, STATION == "Station1")
events_st2 <- filter(heat_stress_events, STATION == "Station2")

# check that function retuned same result for both STATIONS
test_that("identify_heat_stress_events() worked for both stations", {

  expect_equal(select(events_st1, -STATION), select(events_st2, -STATION))

})

# check that stress_start is in exceed and ONLY from exceed
check_event1 <- filter(events_st1, SEASON == "S1", DEPTH == 2)
check_event2 <- filter(events_st1, SEASON == "S1",  DEPTH == 5)
check_event3 <- filter(events_st1, SEASON == "S2", DEPTH == 2)
check_event4 <- filter(events_st1, SEASON == "S2",  DEPTH == 5)

test_that("identify_heat_stress_events() finds correct stress_start", {

  expect_true(all(check_event1$stress_start %in% exceed1))
  expect_false(any(!(check_event1$stress_start %in% exceed1)))

  expect_true(all(check_event2$stress_start %in% exceed2))
  expect_false(any(!(check_event2$stress_start %in% exceed2)))

  expect_true(all(check_event3$stress_start %in% exceed3))
  expect_false(any(!(check_event3$stress_start %in% exceed3)))

  expect_true(all(check_event4$stress_start %in% exceed4))
  expect_false(any(!(check_event4$stress_start %in% exceed4)))

})

# check that heat stress events do no overlap
check_overlaps <- events_st1 %>%
  group_by(STATION, DEPTH) %>%
  mutate(LEAD_END = dplyr::lead(stress_end), OVERLAP = LEAD_END < stress_end)

test_that("heat stress events do not overlap", {

  expect_false(unique(na.omit(check_overlaps$OVERLAP)))

})

# check that event lengths match what is expected
test_that("length of heat stress events matches expected values", {

  expect_equal(as.numeric(max(heat_stress_events$DIFF)), 48)
  expect_equal(as.numeric(min(heat_stress_events$DIFF)), 24)
  expect_equal(round(as.numeric(mean(heat_stress_events$DIFF)), digits = 2), 26.38)

})


# filter_out_heat_stress_events --------------------------------------------

dat_filt <- filter_out_heat_stress_events(dat)

# plot_temperature_at_depth(dat_filt, facet_var = "STATION + DEPTH")

# filtered data for each STATION and DEPTH
filt_st1 <- filter(dat_filt, STATION == "Station1") %>% arrange(TIMESTAMP)
filt_st2 <- filter(dat_filt, STATION == "Station2") %>% arrange(TIMESTAMP)

# check that function returned same result for both STATIONS
test_that("filter_out_heat_stress_events() worked for both stations",{

  expect_equal(select(filt_st1, -STATION), select(filt_st2, -STATION))

})

# check there are
test_that("filter_out_heat_stress_events() removes values >= threshold", {

  expect_equal(nrow(filter(filt_st1, VALUE >= 18)), 0)

})

# heat stress *intervals* for each STATION & DEPTH to use in data.table function
check1 <- check_int1 %>% select(-STATION, -DEPTH, -SEASON, -DIFF)
dat1 <- dat_filt %>%
  filter(DEPTH == 2, SEASON == "S1") %>%
  select(-STATION, -DEPTH, -SEASON)

check2 <- check_int2 %>% select(-STATION, -DEPTH, -SEASON,  -DIFF)
dat2 <- dat_filt %>%
  filter(DEPTH == 5, SEASON == "S1") %>%
  select(-STATION, -DEPTH, -SEASON)

check3 <- check_int3 %>% select(-STATION, -DEPTH, -SEASON, -DIFF)
dat3 <- dat_filt %>%
  filter(DEPTH == 2, SEASON == "S2") %>%
  select(-STATION, -DEPTH, -SEASON)

check4 <- check_int4 %>% select(-STATION, -DEPTH,-SEASON,  -DIFF)
dat4 <- dat_filt %>%
  filter(DEPTH == 5, SEASON == "S2") %>%
  select(-STATION, -DEPTH, -SEASON)


test_that("filter_out_heat_stress_events() removes all heat stress events", {

  expect_equal(nrow(setDT(dat1)[TIMESTAMP %inrange% check1]), 0)
  expect_equal(nrow(setDT(dat2)[TIMESTAMP %inrange% check2]), 0)
  expect_equal(nrow(setDT(dat3)[TIMESTAMP %inrange% check3]), 0)
  expect_equal(nrow(setDT(dat4)[TIMESTAMP %inrange% check4]), 0)

})




