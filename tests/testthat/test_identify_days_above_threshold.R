context("Filter")
library(tgc)
library(dplyr)
library(lubridate)

# set up ------------------------------------------------------------------

data(string_data)

dat <- string_data %>%
  filter(VARIABLE == "Temperature") %>%
  select(-SENSOR, -DEPLOYMENT_PERIOD, -VARIABLE)

# Case 1: Error message -------------------------------------------------------------------

test_that("more than one unique value in the VARIABLE column causes an error",{

  expect_error(identify_days_above_threshold(string_data))

})

# Case 2: 0 Extra days ----------------------------------------------------------------

thresh <- 18.5
min_exceed <- 1
extra_days <- 0

# dates that should be filtered out for each depth
to_remove <- dat %>%
  mutate(
    DATE = as_date(TIMESTAMP),
    EXCEED_THRESH = if_else(VALUE >= thresh, TRUE, FALSE)
  ) %>%
  group_by(DEPTH, DATE) %>%
  summarise(n_obs = sum(EXCEED_THRESH)) %>%
  filter(n_obs >= min_exceed) %>%
  ungroup()

# function
remove_foo <- identify_days_above_threshold(
  dat,
  threshold = thresh,
  min_exceedance = min_exceed,
  n_extra_days = extra_days
)

test_that("function identifies correct dates for n_extra_days = 0",{

  expect_equal(to_remove, remove_foo)

})


# Case 2: 1 Extra day ----------------------------------------------------------------

thresh <- 18.5
min_exceed <- 1
extra_days <- 1

# dates that should be filtered out for each depth
to_remove0 <- dat %>%
  mutate(
    DATE = as_date(TIMESTAMP),
    EXCEED_THRESH = if_else(VALUE >= thresh, TRUE, FALSE)
  ) %>%
  group_by(DEPTH, DATE) %>%
  summarise(n_obs = sum(EXCEED_THRESH)) %>%
  filter(n_obs >= min_exceed) %>%
  ungroup()

to_remove <- to_remove0 %>%
  rbind(
    to_remove0 %>% mutate(DATE = DATE + lubridate::days(extra_days))
  ) %>%
  distinct(DATE, DEPTH) %>%
  arrange(DEPTH, DATE) %>%
  left_join(to_remove0, by = c("DEPTH", "DATE"))

# function
remove_foo <- identify_days_above_threshold(
  dat,
  threshold = thresh,
  min_exceedance = min_exceed,
  n_extra_days = extra_days
)

test_that("function identifies correct dates for n_extra_days = 1",{

  expect_equal(to_remove, remove_foo)

})



# Case 3: 2 Extra days ----------------------------------------------------------------

thresh <- 18.5
min_exceed <- 1
extra_days <- 2

# dates that should be filtered out for each depth
to_remove0 <- dat %>%
  mutate(
    DATE = as_date(TIMESTAMP),
    EXCEED_THRESH = if_else(VALUE >= thresh, TRUE, FALSE)
  ) %>%
  group_by(DEPTH, DATE) %>%
  summarise(n_obs = sum(EXCEED_THRESH)) %>%
  filter(n_obs >= min_exceed) %>%
  ungroup()

to_remove_1 <- to_remove0 %>% mutate(DATE = DATE + lubridate::days(1))
to_remove_2 <- to_remove0 %>% mutate(DATE = DATE + lubridate::days(2))

to_remove <- to_remove0 %>%
  rbind(to_remove_1) %>%
  rbind(to_remove_2) %>%
  distinct(DATE, DEPTH) %>%
  arrange(DEPTH, DATE) %>%
  left_join(to_remove0, by = c("DEPTH", "DATE"))

# function
remove_foo <- identify_days_above_threshold(
  dat,
  threshold = thresh,
  min_exceedance = min_exceed,
  n_extra_days = extra_days
)

test_that("function identifies correct dates for n_extra_days = 2",{

  expect_equal(to_remove, remove_foo)

})

# Case 4: > 1 min_exceed ----------------------------------------------------------------

thresh <- 18.5
min_exceed <- 10
extra_days <- 0

# dates that should be filtered out for each depth
to_remove <- dat %>%
  mutate(
    DATE = as_date(TIMESTAMP),
    EXCEED_THRESH = if_else(VALUE >= thresh, TRUE, FALSE)
  ) %>%
  group_by(DEPTH, DATE) %>%
  summarise(n_obs = sum(EXCEED_THRESH)) %>%
  filter(n_obs >= min_exceed) %>%
  ungroup()

# function
remove_foo <- identify_days_above_threshold(
  dat,
  threshold = thresh,
  min_exceedance = min_exceed,
  n_extra_days = extra_days
) %>%
  arrange(DEPTH, DATE)

test_that("function identifies correct dates for min_exceed > 1",{

  expect_equal(to_remove, remove_foo)

})


