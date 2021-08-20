library(lubridate)
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

set.seed(18)
exceed2 <- sample(TIMESTAMP$TIMESTAMP, 10)

set.seed(4769)
exceed3 <- sample(TIMESTAMP$TIMESTAMP, 10)

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
check1 <- heat_stress_int %>% filter(STATION == "Station1", DEPTH == 2)
check2 <- heat_stress_int %>% filter(STATION == "Station1", DEPTH == 5)
check3 <- heat_stress_int %>% filter(STATION == "Station2", DEPTH == 10)
check4 <- heat_stress_int %>% filter(STATION == "Station2", DEPTH == 15)


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

heat_stress_events <- identify_heat_stress_events(dat, STATION) %>%
  mutate(DIFF = difftime(stress_end, stress_start, unit = "hour"))

# check that stress_start is in exceed and ONLY from exceed
check1 <- heat_stress_events %>% filter(STATION == "Station1", DEPTH == 2)
check2 <- heat_stress_events %>% filter(STATION == "Station1", DEPTH == 5)
check3 <- heat_stress_events %>% filter(STATION == "Station2", DEPTH == 10)
check4 <- heat_stress_events %>% filter(STATION == "Station2", DEPTH == 15)

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
  mutate(LEAD_END = dplyr::lead(stress_end),
         OVERLAP = LEAD_END < stress_end)

test_that("heat stress events do not overlap", {

  expect_false(unique(na.omit(check_overlaps$OVERLAP)))

})


# st_filter_heat_stress_events --------------------------------------------

dat_filt <- dat %>% st_filter_out_heat_stress_events()

plot_temperature_at_depth(dat_filt, facet_var = "STATION + DEPTH")



# check Station2 ----------------------------------------------------------

dat2 <- dat %>% filter(STATION == "Station2", DEPTH == 10)

int <- dat2 %>% identify_heat_stress_intervals()

events <- dat2 %>% identify_heat_stress_events() %>%
  mutate(DIFF = difftime(stress_end, stress_start, unit = "hour"))






