# October 28, 2021

# Generate data to test *_season, *_heat_stress, and *count_ functions
# Two stations, with two depths and two seasons each.
# Data for Station1 and Station2 is the same

# test_season_functions.R:
# To trigger a new season from identify_trending_up() and identify_growing_seasons():
## lag(VALUE) < trend_threshold & VALUE >= trend_threshold (default trend_threshold = 4)
## For test data:
### the first observation for S1, DEPTH = 2 is 4 deg C, which DOES NOT trigger a season start
### the first observation for S1, DEPTH = 5 is 3 deg C, which DOES trigger season start

# To trigger end of season from identify_first_superchill() and identify_growing_seasons():
## lag(VALUE) > superchill_threshold & VALUE <= superchill_threshold (default superchill_threshold = -0.7)
## Observations to trigger end of season are added in test_season_functions.R:
## Observations of -0.7 deg C at the end of February for DEPTH = 2 m, which DOES trigger a season end
## no superchill for DEPTH = 5 m, which DOES NOT trigger a season end

## For test_heat_stress_functions.R:
# Values are added at known TIMESTAMPS to trigger heat stress functions
# To trigger heat stress interval, VALUE >= heat_threshold (default heat_threshold = 18)
# For S1, DEPTH = 2, observations of 18 deg C are added, which DO trigger heat stress

# S1, DEPTH = 2: 9 heat stress intervals, 6 heat stress events
## Note: two of the above heat stress intervals are 24 hours apart
## and result in 1 48-hour heat stress event
# S1, DEPTH = 5: 5 heat stress intervals, 5 heat stress events

# S2, DEPTH = 2: 5 heat stress intervals, 5 heat stress events
# S2, DEPTH = 5: 6 heat stress intervals, 5 heat stress events


# CODE --------------------------------------------------------------------

library(tgc)
library(dplyr)
library(lubridate)


# SEASON 1 ----------------------------------------------------------------

# 15-minute TIMESTAMP for 1 month
# start 15 mins before July 1 so SEASON begins July 1
S1 <- tibble(TIMESTAMP = seq(as_datetime("2021-06-30 23:45:00"),
                             as_datetime("2021-07-31"),
                             by = "15 min"),
             SEASON = "S1")

# TIMESTAMPs for heat stress
exceed1 <- S1$TIMESTAMP[seq(50, nrow(S1), by = 500)]
# add overlapping intervals
exceed1 <- c(exceed1, exceed1[c(1, 4, 6)] + hours(c(1, 12, 24)))

exceed2 <- S1$TIMESTAMP[seq(10, nrow(S1), by = 600)]

# fake data for S1, 2 DEPTHs
set.seed(109)
dat_S1 <- S1 %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 14, 0.5)) %>%
  rbind(
    S1 %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 10, 1))
  ) %>%
  # add heat stress & season trigger
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% exceed1 ~ 18,
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP == min(TIMESTAMP) ~ 4,

      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% exceed2 ~ 20,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP == min(TIMESTAMP) ~ 3,

      TRUE ~ VALUE)
  )

#plot_temperature_at_depth(dat_S1)


# SEASON 2 -----------------------------------------------------------------

# 1 month of data in February
S2 <- tibble(TIMESTAMP = seq(as_datetime("2022-01-31 23:45:00"),
                             as_datetime("2022-02-28"),
                             by = "15 min"),
             SEASON = "S2")

# TIMESTAMPs for heat stress
exceed3 <- S2$TIMESTAMP[seq(100, nrow(S2), by = 550)]

exceed4 <- S2$TIMESTAMP[seq(30, nrow(S2), by = 600)]
# add overlapping intervals
exceed4 <- c(exceed4, exceed4[4] + hours(13))

#  data for 1 STATION, DEPTH and SEASON
set.seed(451)
dat_S2 <- S2 %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 12, 0.5)) %>%
  rbind(
    S2 %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 11, 1))
  ) %>%
  # add exceedences & season trigger
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% exceed3 ~ 19,
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP == min(TIMESTAMP) ~ 3.9,

      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% exceed4 ~ 22,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP == min(TIMESTAMP) ~ 3,

      TRUE ~ VALUE)
  )

#plot_temperature_at_depth(dat_S2)


# Duplicate for Station2 --------------------------------------------------

dat <- rbind(dat_S1, dat_S2) %>%
  rbind(
    rbind(dat_S1, dat_S2) %>%
      mutate(STATION = "Station2")
  )

rm(dat_S1, dat_S2, S1, S2)

#plot_temperature_at_depth(dat, facet_var = "STATION")



