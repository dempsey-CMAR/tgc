library(tgc)
library(dplyr)
library(lubridate)

# 15-minute TIMESTAMP for 1 month
TIMESTAMP <- tibble(TIMESTAMP = seq(as_datetime("2021-07-01"),
                                    as_datetime("2021-07-31"),
                                    by = "15 min"))


# TIMESTAMP for exceedences for each STATION and DEPTH
set.seed(189)
exceed1 <- sample(TIMESTAMP$TIMESTAMP, size = 10)
# add some overlapping intervals
set.seed(189)
exceed1.1 <- sample(exceed1, size = 5) + hours(12)
exceed1 <- c(exceed1, exceed1.1)

exceed2 <- sample(TIMESTAMP$TIMESTAMP, 10)

exceed3 <- sample(TIMESTAMP$TIMESTAMP, 10)
# add some overlapping intervals
exceed3.1 <- sort(exceed3)[1:5] + hours(12)
exceed3 <- c(exceed3, exceed3.1)

exceed4 <- sample(TIMESTAMP$TIMESTAMP, 10)


# fake data for 2 STATIONs, 2 DEPTHs each, with all values < 18
set.seed(1809)
dat1 <- TIMESTAMP %>%
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
  ) %>%
  # add exceedences
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% exceed1 ~ 18,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% exceed2 ~ 18.5,

      STATION == "Station2" & DEPTH == 10 & TIMESTAMP %in% exceed3 ~ 20,
      STATION == "Station2" & DEPTH == 15 & TIMESTAMP %in% exceed4 ~ 22,

      TRUE ~ VALUE),
    SEASON = "S1"
  )


#plot_temperature_at_depth(dat1, facet_var = "STATION + DEPTH")

# Add another season
TIMESTAMP2 <- tibble(TIMESTAMP = seq(as_datetime("2022-01-31 23:45:00"),
                                     as_datetime("2022-02-28"),
                                     by = "15 min"))


# TIMESTAMP for exceedences
exceed_S2 <- c(TIMESTAMP2$TIMESTAMP[100], TIMESTAMP2$TIMESTAMP[1574], TIMESTAMP2$TIMESTAMP[270])

# fake data for 1 STATION, DEPTH and SEASON
set.seed(451)
dat2 <- TIMESTAMP2 %>%
  mutate(
    STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 10, 1.5),
    VALUE = case_when(
      TIMESTAMP %in% exceed_S2 ~ 20,
      TIMESTAMP == min(TIMESTAMP) ~ 3,
      TRUE ~ VALUE),
    SEASON = "S2"
  )

dat <- rbind(dat1, dat2)
