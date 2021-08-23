library(lubridate)
library(data.table)
library(dplyr)
library(tgc)


# 15-minute TIMESTAMP for 1 month
TIMESTAMP <- tibble(
  TIMESTAMP = seq(
    as_datetime("2021-07-01"), as_datetime("2021-07-31"), by = "15 min"
  )
)

# fake data for 2 STATIONs, 2 DEPTHs each, with all values < 18
set.seed(1809)
dat <- TIMESTAMP %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 8, 2)) %>%
  rbind(
    TIMESTAMP %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 6, 3))
  ) %>%
  rbind(
    TIMESTAMP %>%
      mutate(STATION = "Station2", DEPTH = 10, VALUE = rnorm(n(), 5, 1))
  ) %>%
  rbind(
    TIMESTAMP %>%
      mutate(STATION = "Station2", DEPTH = 15, VALUE = rnorm(n(), 5, 2))
  )

max(dat$VALUE)

# TIMESTAMP for exceedances for each STATION and DEPTH
set.seed(189)
exceed1 <- sample(TIMESTAMP$TIMESTAMP, size = 10)
# add some overlapping intervals
set.seed(189)
exceed1.1 <- sample(exceed1, size = 5) + hours(12)
exceed1 <- c(exceed1, exceed1.1)

set.seed(18)
exceed2 <- sample(TIMESTAMP$TIMESTAMP, 10)

set.seed(4769)
exceed3 <- sample(TIMESTAMP$TIMESTAMP, 10)
# add some overlapping intervals
set.seed(4769)
exceed3.1 <- sort(exceed3)[1:5] + hours(12)
exceed3 <- c(exceed3, exceed3.1)

set.seed(76)
exceed4 <- sample(TIMESTAMP$TIMESTAMP, 10)

# add exceedences
dat <- dat %>%
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% exceed1 ~ 18,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% exceed2 ~ 18.5,

      STATION == "Station2" & DEPTH == 10 & TIMESTAMP %in% exceed3 ~ 20,
      STATION == "Station2" & DEPTH == 15 & TIMESTAMP %in% exceed4 ~ 22,

      TRUE ~ VALUE)
  )

plot_temperature_at_depth(dat, facet_var = "STATION + DEPTH")


# identify_heat_stress_interval -------------------------------------------

heat_stress_int <- identify_heat_stress_intervals(dat, STATION)  %>%
  mutate(DIFF = difftime(interval_end, interval_start, unit = "hour"))

# exceedences identified by function for each STATION and DEPTH
check1 <- filter(heat_stress_int, STATION == "Station1", DEPTH == 2)
check2 <- filter(heat_stress_int, STATION == "Station1", DEPTH == 5)
check3 <- filter(heat_stress_int, STATION == "Station2", DEPTH == 10)
check4 <- filter(heat_stress_int, STATION == "Station2", DEPTH == 15)


# check that all TIMESTAMPs identified by function are in exceed &
# function identified all TIMESTAMPs from exceed
test_that("identify_heat_stress_intervals() finds correct interval_start",{

  expect_equal(check1$interval_start, sort(exceed1))
  expect_equal(check2$interval_start, sort(exceed2))
  expect_equal(check3$interval_start, sort(exceed3))
  expect_equal(check4$interval_start, sort(exceed4))

})


test_that("identify_heat_stress_intervals() finds correct interval_end",{

  expect_equal(unique(heat_stress_int$DIFF), 24)

})


# identify_heat_stress_events ---------------------------------------------

heat_stress_events <- dat %>%
  identify_heat_stress_events(STATION) %>%
  mutate(DIFF = difftime(stress_end, stress_start, unit = "hour"))

# check that stress_start is in exceed and ONLY from exceed
check1 <- filter(heat_stress_events, STATION == "Station1", DEPTH == 2)
check2 <- filter(heat_stress_events, STATION == "Station1", DEPTH == 5)
check3 <- filter(heat_stress_events, STATION == "Station2", DEPTH == 10)
check4 <- filter(heat_stress_events, STATION == "Station2", DEPTH == 15)

test_that("identify_heat_stress_events() finds correct stress_start", {

  expect_true(all(check1$stress_start %in% exceed1))
  expect_false(any(!(check1$stress_start %in% exceed1)))

  expect_true(all(check2$stress_start %in% exceed2))
  expect_false(any(!(check2$stress_start %in% exceed2)))

  expect_true(all(check3$stress_start %in% exceed3))
  expect_false(any(!(check3$stress_start %in% exceed3)))

  expect_true(all(check4$stress_start %in% exceed4))
  expect_false(any(!(check4$stress_start %in% exceed4)))

})

# check that heat stress events do no overlap
check_overlaps <- heat_stress_events %>%
  group_by(STATION, DEPTH) %>%
  mutate(LEAD_END = dplyr::lead(stress_end), OVERLAP = LEAD_END < stress_end)

test_that("heat stress events do not overlap", {

  expect_false(unique(na.omit(check_overlaps$OVERLAP)))

})

# check that event lengths match what is expected
test_that("legnth of heat stress events matches expected values", {

  expect_equal(as.numeric(max(heat_stress_events$DIFF)), 56.5)
  expect_equal(as.numeric(min(heat_stress_events$DIFF)), 24)
  expect_equal(round(as.numeric(mean(heat_stress_events$DIFF)), digits = 2),
               30.62)

})


# st_filter_out_heat_stress_events --------------------------------------------

dat_filt <- dat %>% st_filter_out_heat_stress_events()

plot_temperature_at_depth(dat_filt, facet_var = "STATION + DEPTH")


# check that obs identified in
test_that("st_filter_out_heat_stress_events() removes values >= threshold", {

  expect_equal(nrow(dat_filt %>% filter(VALUE >= 18)), 0)


})


check1 <- check1 %>% select(-STATION, -DEPTH, -event_id, -DIFF)
dat1 <- dat_filt %>%
  filter(STATION == "Station1", DEPTH == 2) %>%
  select(-STATION, -DEPTH)

check2 <- check2 %>% select(-STATION, -DEPTH, -event_id, -DIFF)
dat2 <- dat_filt %>%
  filter(STATION == "Station1", DEPTH == 5) %>%
  select(-STATION, -DEPTH)

check3 <- check3 %>% select(-STATION, -DEPTH, -event_id, -DIFF)
dat3 <- dat_filt %>%
  filter(STATION == "Station2", DEPTH == 10) %>%
  select(-STATION, -DEPTH)

check4 <- check4 %>% select(-STATION, -DEPTH, -event_id, -DIFF)
dat4 <- dat_filt %>%
  filter(STATION == "Station2", DEPTH == 15) %>%
  select(-STATION, -DEPTH)


test_that("st_filter_out_heat_stress_events() removes all heat stress events", {

  expect_equal(nrow(setDT(dat1)[TIMESTAMP %inrange% check1]), 0)
  expect_equal(nrow(setDT(dat2)[TIMESTAMP %inrange% check2]), 0)
  expect_equal(nrow(setDT(dat3)[TIMESTAMP %inrange% check3]), 0)
  expect_equal(nrow(setDT(dat4)[TIMESTAMP %inrange% check4]), 0)


})




