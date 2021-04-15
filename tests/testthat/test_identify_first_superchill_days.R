library(tgc)
library(dplyr)
library(lubridate)

# Set up ------------------------------------------------------------------

data("string_data")

dat1 <- string_data %>%
  filter(VARIABLE == "Temperature") %>%
  select(-SENSOR, -DEPLOYMENT_PERIOD) %>%
  mutate(VALUE = VALUE - 6)
dat1$TIMESTAMP <- rev(dat1$TIMESTAMP)


# shuffle to make sure order does not matter
set.seed(468)
rows <- sample(nrow(dat1))
dat1_shuffle <- dat1[rows, ]

dat2 <- dat1
year(dat2$TIMESTAMP) <- 2020

set.seed(685)
rows <- sample(nrow(dat2))
dat2_shuffle <- dat2[rows, ]

dat <- rbind(dat1_shuffle, dat2_shuffle)


lower_threshold <- -0.7

rm(dat1, dat2)

# Case 1: Group by DEPTH --------------------------------------------------
ggplot_variables_at_depth(dat1) +
  geom_hline(yintercept = lower_threshold, col = "red")

superchill <- dat1_shuffle %>%
  mutate(YEAR = year(TIMESTAMP)) %>%
  group_by(DEPTH) %>%
  arrange(TIMESTAMP, .by_group = TRUE) %>%
  mutate(CROSS_THRESH = if_else(
    # might need to switch these signs
    lag(VALUE) < lower_threshold & VALUE >= lower_threshold, TRUE, FALSE )
  ) %>%
  filter(CROSS_THRESH) %>%
  summarise(START_TREND = min(TIMESTAMP)) %>%
  ungroup()

superchill_foo <- identify_first_superchill_days(
  dat1_shuffle,
  lower_threshold = lower_threshold,
  DEPTH
)

test_that("function identifies correct first superchill dates when grouped by DEPTH",{

  expect_equal(superchill, superchill_foo)

})

# Case 2: Group by YEAR and DEPTH  --------------------------------------------------

trend_up <- dat %>%
  mutate(YEAR = year(TIMESTAMP)) %>%
  group_by(YEAR, DEPTH) %>%
  arrange(TIMESTAMP, .by_group = TRUE) %>%
  mutate(CROSS_THRESH = if_else(
    lag(VALUE) < lower_threshold & VALUE >= lower_threshold, TRUE, FALSE )
  ) %>%
  filter(CROSS_THRESH) %>%
  summarise(START_TREND = max(TIMESTAMP)) %>%
  ungroup()

trend_up_foo <- identify_trending_up_days(
  dat,
  lower_threshold = lower_threshold,
  YEAR, DEPTH
)

test_that("function identifies correct dates when grouped by YEAR and DEPTH",{

  expect_equal(trend_up, trend_up_foo)

})

