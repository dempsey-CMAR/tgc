library(tgc)
library(dplyr)
library(lubridate)

# SEASON 1: 15-minute TIMESTAMP for 1 month
# start 15 mins before July 1 so SEASON begins July 1
S1 <- tibble(TIMESTAMP = seq(as_datetime("2021-06-30 23:45:00"),
                             as_datetime("2021-07-31"),
                             by = "15 min"),
             SEASON = "S1")

# TIMESTAMP for heat stress
exceed1 <- S1$TIMESTAMP[seq(50, nrow(S1), by = 500)]
exceed1 <- c(exceed1, exceed1[c(1, 4, 6)] + hours(c(1, 12, 24))) # overlapping intervals

exceed2 <- S1$TIMESTAMP[seq(10, nrow(S1), by = 600)]

# SEASON 2: 1 month of data in January
# this will be counted as the END of S1 and the
S2 <- tibble(TIMESTAMP = seq(as_datetime("2022-01-31 23:45:00"),
                             as_datetime("2022-02-28"),
                             by = "15 min"),
             SEASON = "S2")

# TIMESTAMP for exceedences for each DEPTH
exceed3 <- S2$TIMESTAMP[seq(100, nrow(S2), by = 550)]

exceed4 <- S2$TIMESTAMP[seq(30, nrow(S2), by = 600)]
# add overlapping intervals
exceed4 <- c(exceed4, exceed4[4] + hours(13))

# fake data for SEASON1, 2 DEPTHs
set.seed(109)
dat_S1 <- S1 %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 14, 0.5)) %>%
  rbind(
    S1 %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 10, 1))
  ) %>%
  # add exceedences & season trigger
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% exceed1 ~ 18,
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP == min(TIMESTAMP) ~ 4,

      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% exceed2 ~ 20,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP == min(TIMESTAMP) ~ 3,

      TRUE ~ VALUE)
  )

#plot_temperature_at_depth(dat_S1)

#  data for 1 STATION, DEPTH and SEASON
set.seed(451)
dat_S2 <- S2 %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 14, 0.5)) %>%
  rbind(
    S2 %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 10, 1))
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

dat <- rbind(dat_S1, dat_S2) %>%
  rbind(
    rbind(dat_S1, dat_S2) %>%
      mutate(STATION = "Station2")
  )

rm(dat_S1, dat_S2, S1, S2)

#plot_temperature_at_depth(dat, facet_var = "STATION")

# SEASON1, DEPTH 2: 9 heat stress intervals, 6 heat stress events
# SEASON1, DEPTH 5: 5 heat stress intervals, 5 heat stress events

# SEASON2, DEPTH 2: 5 heat stress intervals, 5 heat stress events
# SEASON2, DEPTH 5: 6 heat stress intervals, 5 heat stress events





