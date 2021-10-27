library(lubridate)
library(data.table)
library(dplyr)
library(tgc)


source(system.file("testdata/test_data.R", package = "tgc"))

# rm(exceed1, exceed2, exceed3, exceed4)


# Partial SEASONs, as defined in dat ---------------------------------------------

# remove first row of each group
dat2 <- dat %>%
  group_by(STATION, SEASON, DEPTH) %>%
  mutate(INDEX = 1:n()) %>%
  filter(INDEX != 1) %>%
  select(-INDEX) %>%
  ungroup()

#plot_temperature_at_depth(dat2, facet_var = "STATION")

n_growing <- count_growing_days(dat2, STATION)

growing_st1 <- filter(n_growing, STATION == "Station1")
growing_st2 <- filter(n_growing, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("count_growing_days() worked for both stations",{

  expect_equal(select(growing_st1, -STATION), select(growing_st2, -STATION))

})

test_that("count_growing_days() outputs expected results",{

  expect_equal(growing_st1$STOCKED_DAYS, c(30, 30, 27, 27))
  # 5 24-hour heat stress intervals/events for S1, DEPTH = 5 and S2 DEPTH = 2
  # Overlapping heat stress intervals result in partial days filtered out
  # for S1 DEPTH = 2 and S2 DEPTH = 5
  expect_equal(growing_st1$n_growing_days, c(22.46, 25.00, 22.00, 21.46))

})

# check that function will assign SEASON if there is no SEASON column
dat3 <- dat2 %>% select(-SEASON)

growing_dat3 <- suppressMessages(count_growing_days(dat3, STATION, full_season = FALSE))

test_that("count_growing_days() adds SEASON column if none exists", {

  expect_message(count_growing_days(dat3, STATION, full_season = FALSE))

  expect_true("SEASON" %in% colnames(growing_dat3))

})


# Degree-Day function -----------------------------------------------------

# expected averages based on how the data was generated:
# S1 2 m: 14 deg C; S1 5 m: 10 deg C
# S2 2 m: 14 deg C S2 5 m: 10 deg C

dd <- count_degree_days(dat2, STATION)

dd_st1 <- filter(dd, STATION == "Station1")
dd_st2 <- filter(dd, STATION == "Station2")

# check that function returned same result for both STATIONS
test_that("count_degree_days() worked for both stations",{

  expect_equal(select(dd_st1, -STATION), select(dd_st2, -STATION))

})

# check that function returns expected output
test_that("count_degree_days() outputs expected avg temp and degree days", {

  expect_equal(round(dd_st1$AVG_TEMPERATURE, digits = 0), c(14, 10, 14, 10))

  expect_equal(round(dd_st1$n_degree_days, digits = 0), c(314, 250, 308, 215))

})

# check that function will assign SEASON if there is no SEASON column
dat3 <- dat2 %>% select(-SEASON)

dd_dat3 <- suppressMessages(count_degree_days(dat3, STATION, full_season = FALSE))

test_that("count_growing_days() adds SEASON column if none exists", {

  expect_message(count_growing_days(dat3, STATION, full_season = FALSE))

  expect_true("SEASON" %in% colnames(growing_dat3))

})

