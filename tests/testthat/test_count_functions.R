library(lubridate)
library(data.table)
library(dplyr)
library(tgc)


# Set up test data where TIMESTAMP of exceedences is specified ------------

# 15-minute TIMESTAMP for 1 month
# start for 15 mins before July 1 so SEASON starts July 1
TIMESTAMP1 <- tibble(TIMESTAMP = seq(as_datetime("2021-06-30 23:45:00"),
                                    as_datetime("2021-07-31"),
                                    by = "15 min"))


# TIMESTAMP for exceedences for each STATION and DEPTH
exceed1 <- c(TIMESTAMP1$TIMESTAMP[10], TIMESTAMP1$TIMESTAMP[1000], TIMESTAMP1$TIMESTAMP[2000])

# fake data for 1 STATION, DEPTH and SEASON
set.seed(1809)
dat1 <- TIMESTAMP1 %>%
  mutate(
    STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 10, 1.5),
    VALUE = case_when(
      TIMESTAMP %in% exceed1 ~ 20,
      TIMESTAMP == min(TIMESTAMP) ~ 3,
      TRUE ~ VALUE)
  )

#plot_temperature_at_depth(dat1)

n_growing <- count_growing_days(dat1, apply_season_filt = TRUE, full_season = FALSE)


test_that("count_growing_seasons() outputs expected results for one season",{

  expect_equal(n_growing$STOCKED_DAYS, 30)
  expect_equal(n_growing$n_growing_days, 27)

})

# Add another season
TIMESTAMP2 <- tibble(TIMESTAMP = seq(as_datetime("2022-01-31 23:45:00"),
                                    as_datetime("2022-02-28"),
                                    by = "15 min"))


# TIMESTAMP for exceedences for each STATION and DEPTH
exceed2 <- c(TIMESTAMP2$TIMESTAMP[100], TIMESTAMP2$TIMESTAMP[1574], TIMESTAMP2$TIMESTAMP[270])

# fake data for 1 STATION, DEPTH and SEASON
set.seed(451)
dat2 <- TIMESTAMP2 %>%
  mutate(
    STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 10, 1.5),
    VALUE = case_when(
      TIMESTAMP %in% exceed2 ~ 20,
      TIMESTAMP == min(TIMESTAMP) ~ 3,
      TRUE ~ VALUE)
  )

dat <- rbind(dat1, dat2)

#plot_temperature_at_depth(dat)

n_growing <- count_growing_days(dat, apply_season_filt = TRUE, full_season = FALSE)


test_that("count_growing_seasons() outputs expected results for two seasons",{

  expect_equal(n_growing$STOCKED_DAYS, c(242, 27))
  expect_equal(n_growing$n_growing_days, c(236, 24))

})


# degree days -------------------------------------------------------------

dd <- count_degree_days(dat1, apply_season_filt = TRUE, full_season = FALSE)

test_that("count_degree_days() outputs expected results for one season",{

  expect_equal(dd$AVG_TEMPERATURE, 10.011)
  expect_equal(dd$n_degree_days, 270.3)

})


dd2 <- count_degree_days(dat, apply_season_filt = TRUE, full_season = FALSE)

test_that("count_degree_days() outputs expected results for two seasons",{

  expect_equal(dd2$AVG_TEMPERATURE, 10.011)
  expect_equal(dd2$n_degree_days, 270.3)

})



