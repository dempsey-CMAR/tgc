library(tgc)
library(data.table)
library(dplyr)
library(lubridate)
library(purrr)
library(strings)
library(tidyr)

# Set up ------------------------------------------------------------------

data(string_data)

dat <- string_data %>%
  filter(VARIABLE == "Temperature",
         TIMESTAMP > as_datetime("2019-08-17"),
         TIMESTAMP <= as_datetime("2019-09-09")) %>%
  select(-SENSOR, -DEPLOYMENT_PERIOD, -VARIABLE) %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP))

thresh <- 18.4
n_hours <- 24

# Run functions -----------------------------------------------------------

intervals <- identify_heat_stress_intervals(dat, threshold = thresh)

events <- identify_heat_stress_events(dat, threshold = thresh)

filtered_data <- filter_heat_stress_events(dat, threshold = thresh)

# format interval and filtered data
intervals_tidy <- pivot_longer(
  intervals,
  cols = c("interval_start", "interval_end"),
  names_to = c("ID"),
  values_to = "TIMESTAMP"
)

int_2 <- intervals_tidy %>%
  filter(DEPTH == "2") %>%
  select(TIMESTAMP) %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP))

filtered_data_2 <- filtered_data %>% filter(DEPTH == "2")

int_5 <- intervals_tidy %>%
  filter(DEPTH == "5") %>%
  select(TIMESTAMP) %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP))

filtered_data_5 <- filtered_data %>% filter(DEPTH == "5")


# Tests -------------------------------------------------------------------

test_that("more than one unique value in the VARIABLE column causes an error",{

  expect_error(identify_heat_stress_intervals(string_data))
  expect_error(identify_heat_stress_events(string_data))
  expect_error(filter_heat_stress_events(string_data, threshold = thresh))

})


test_that("all values above the threshold were filtered out of dat",{

  expect_equal(nrow(filter(filtered_data, VALUE >= thresh)), 0)

})


test_that("interval_start and interval_end are class POSIXct",{

  expect_s3_class(intervals$interval_start, "POSIXct")
  expect_s3_class(intervals$interval_end, "POSIXct")

})


test_that("stress_start and stress_end are class POSIXct",{

  expect_s3_class(events$stress_start, "POSIXct")
  expect_s3_class(events$stress_end, "POSIXct")

})


test_that("interval timestamps are not in filtered data",{

  expect_false(any(int_2$TIMESTAMP %in% filtered_data_2$TIMESTAMP))

  expect_false(any(int_5$TIMESTAMP %in% filtered_data_5$TIMESTAMP))

})










