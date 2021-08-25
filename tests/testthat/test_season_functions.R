library(tgc)
library(dplyr)
library(lubridate)

# Set up ------------------------------------------------------------------

## dat_trend ---------------------------------------------------------------

TIMESTAMP_trend <- tibble(TIMESTAMP = seq(as_datetime("2018-06-01"),
                                          as_datetime("2018-06-30"),
                                          by = "30 mins"))

# TIMESTAMP for crossing 4-degree thresh for each STATION and DEPTH
set.seed(189)
trend1 <- sample(head(TIMESTAMP_trend$TIMESTAMP, n = 50), size = 20)
trend2 <- sample(head(TIMESTAMP_trend$TIMESTAMP, n = 50), size = 20)
trend4 <- sample(head(TIMESTAMP_trend$TIMESTAMP, n = 50), size = 20)

# fake data for 2 STATIONs, 2 DEPTHs each, with all values > 4 deg
set.seed(1809)
dat_trend <- TIMESTAMP_trend %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 18, 2))  %>%
  rbind(
    TIMESTAMP_trend %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 15, 1))
  ) %>%
  rbind(
    TIMESTAMP_trend %>%
      mutate(STATION = "Station2", DEPTH = 10, VALUE = rnorm(n(), 16, 2))
  ) %>%
  rbind(
    TIMESTAMP_trend %>%
      mutate(STATION = "Station2", DEPTH = 15, VALUE = rnorm(n(), 12, 0.5))
  ) %>%
  # add values < 4 degrees for trending up function
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% trend1 ~ rnorm(n(), 4, 2),
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% trend2 ~ rnorm(n(), 4, 2),
      STATION == "Station2" & DEPTH == 15 & TIMESTAMP %in% trend4 ~ rnorm(n(), 3, 1),

      TRUE ~ VALUE),

    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP == max(trend1) ~ 3,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP == max(trend2) ~ 4, # this counts as crossing threshold
      STATION == "Station1" & DEPTH == 5 &
        TIMESTAMP == max(sort(trend2)[-length(trend2)]) ~ 3.9,            # this is the TIMESTAMP to look for

      STATION == "Station2" & DEPTH == 15 & TIMESTAMP == max(trend4) ~ 3.9,

      TRUE ~ VALUE)
  )

min(dat_trend$VALUE)

# shuffle to make sure order does not matter
set.seed(468)
rows <- sample(nrow(dat_trend))
dat_trend <- dat_trend[rows, ]

plot_temperature_at_depth(dat_trend, facet_var = "STATION")


# dat_chill ---------------------------------------------------------------

TIMESTAMP_chill <- tibble(TIMESTAMP = seq(as_datetime("2019-02-01"),
                                          as_datetime("2019-02-25"),
                                          by = "30 mins"))

# TIMESTAMP for crossing 4-degree thresh for each STATION and DEPTH
set.seed(189)
chill1 <- sample(tail(TIMESTAMP_chill$TIMESTAMP, n = 50), size = 20)
chill2 <- sample(tail(TIMESTAMP_chill$TIMESTAMP, n = 50), size = 20)
chill3 <- sample(tail(TIMESTAMP_chill$TIMESTAMP, n = 50), size = 20)
chill4 <- sample(tail(TIMESTAMP_chill$TIMESTAMP, n = 50), size = 20)

# fake data for 2 STATIONs, 2 DEPTHs each, with all values > -0.7 deg
set.seed(1809)
dat_chill <- TIMESTAMP_chill %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 18, 2))  %>%
  rbind(
    TIMESTAMP_chill %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 15, 1))
  ) %>%
  rbind(
    TIMESTAMP_chill %>%
      mutate(STATION = "Station2", DEPTH = 10, VALUE = rnorm(n(), 16, 2))
  ) %>%
  rbind(
    TIMESTAMP_chill %>%
      mutate(STATION = "Station2", DEPTH = 15, VALUE = rnorm(n(), 12, 0.5))
  ) %>%
  # add values < -0.7 degrees for superchill function
  mutate(
    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP %in% chill1 ~ rnorm(n(), -1, 2),
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP %in% chill2 ~ rnorm(n(), 4, 1),
      STATION == "Station2" & DEPTH == 10 & TIMESTAMP %in% chill3 ~ rnorm(n(), 0, 3),
      STATION == "Station2" & DEPTH == 15 & TIMESTAMP %in% chill4 ~ rnorm(n(), -0.7, 1),

      TRUE ~ VALUE),

    VALUE = case_when(
      STATION == "Station1" & DEPTH == 2 & TIMESTAMP == min(chill1) ~ -1,
      STATION == "Station1" & DEPTH == 5 & TIMESTAMP == max(chill2) ~ 5,    # start of S2 (not superchill)
      STATION == "Station2" & DEPTH == 10 & TIMESTAMP == min(chill3) ~ -0.7,
      STATION == "Station2" & DEPTH == 15 & TIMESTAMP == min(chill4) ~ -2,

      TRUE ~ VALUE)
  )

min(dat_chill$VALUE)

# shuffle to make sure order does not matter
set.seed(468)
rows <- sample(nrow(dat_chill))
dat_chill <- dat_chill[rows, ]

plot_temperature_at_depth(dat_chill, facet_var = "STATION")


# dat_season --------------------------------------------------------------

# add values < 4 deg C in Sept - Dec (should NOT be identified for START_SEASON)
set.seed(333)
TIMESTAMP_season <- tibble(
  TIMESTAMP = sample(
    seq(as_datetime("2018-09-01"), as_datetime("2018-12-31"), by = "30 mins"),
    size = 100
  )) %>%
  mutate(STATION = "Station1", DEPTH = 2, VALUE = rnorm(n(), 5, 3))


TIMESTAMP_S2 <- tibble(
  TIMESTAMP = seq(as_datetime("2020-08-15"),
                  as_datetime("2020-08-30"),
                  by = "30 mins"))

set.seed(333)
dat_season <- rbind(dat_trend, dat_chill, TIMESTAMP_season) %>%
  rbind(
    TIMESTAMP_season %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 2, 1))
  ) %>%
  rbind(
    TIMESTAMP_season %>%
      mutate(STATION = "Station2", DEPTH = 10, VALUE = rnorm(n(), 4, 2))
  ) %>%
  rbind(
    TIMESTAMP_season %>%
      mutate(STATION = "Station2", DEPTH = 15, VALUE = rnorm(n(), -1, 5))
  )


plot_temperature_at_depth(dat_season, facet_var = "STATION")

dat_season2 <- dat_season %>%
  rbind(
    TIMESTAMP_S2 %>%
      mutate(STATION = "Station1", DEPTH = 5, VALUE = rnorm(n(), 15, 1))
  )


#  identify_trending_up() --------------------------------------------------

# START_TREND is the first obs AFTER crosses threshold so
# subtract 30 mins to get last observation before crossing threshold
trend_up <- identify_trending_up(dat_trend, STATION) %>%
  mutate(trend_compare = START_TREND - minutes(30))

# expected TIMESTAMP of the last observation < 4 degrees for each STATION + DEPTH
trend_match <- c(max(trend1),
                 # VALUE at max(trend2) is 4, which is the beginning of the trend,
                 # so we are looking for the second last timestamp in trend2
                 max(sort(trend2)[-length(trend2)]),
                 # Station2 DEPTH =10 never goes below 4-degrees,
                 # so START_TREND should be NA
                 NA,
                 max(trend4))


test_that("identify_trending_up() outputs expected results",{

  expect_equal(trend_up$trend_compare, trend_match)

})


#  identify_first_superchill() --------------------------------------------------

# FIRST_CHILL is 1 minute before first observation of superchill
chill <- identify_first_superchill(dat_chill, STATION) %>%
  mutate(chill_compare = FIRST_CHILL + minutes(1))

# expected TIMESTAMP of the first observation < -0.7 for each STATION + DEPTH
chill_match <- c(min(chill1), NA, min(chill3), min(chill4))


test_that("identify_first_superchill() outputs expected results",{

  expect_equal(chill$chill_compare, chill_match)

})

# identify_growing_seasons ------------------------------------------------

seasons_full_S1 <- identify_growing_seasons(dat_season, STATION) %>%
  filter(SEASON == "S1")

test_that("identify_growing_seasons() identifies expected START_SEASON and END_SEASON", {

  expect_equal(seasons_full_S1$START_SEASON, trend_up$START_TREND)
  expect_equal(seasons_full_S1$END_SEASON, chill$FIRST_CHILL)

})

seasons_full_S2 <- identify_growing_seasons(dat_season2, STATION) %>%
  filter(STATION == "Station1", DEPTH == 5) %>%
  na.omit()

check1 <- seasons_full_S2[which(seasons_full_S2$SEASON == "S1"), "SEASON_DAYS"]$SEASON_DAYS
check2 <- seasons_full_S2[which(seasons_full_S2$SEASON == "S2"), "SEASON_DAYS"]$SEASON_DAYS

test_that("identify_growing_seasons() found two full seasons for Station1 at 5 m", {

  expect_equal(sort(unique(seasons_full_S2$SEASON)), c("S1", "S2"))

  expect_equal(check1, 540)
  expect_equal(check2, 540)
})


seasons_all <- identify_growing_seasons(dat_season2, STATION,
                                        full_season = FALSE) %>%
  filter(SEASON == "S2", !(STATION == "Station1" & DEPTH == 5))

test_that("identify_growing_seasons(full_season = FALSE) fills in last TIMESTAMP for END_SEASON", {

  expect_equal(unique(seasons_all$END_SEASON), max(dat_season$TIMESTAMP))

})

