library(tgc)
library(dplyr)
library(lubridate)

# Set up ------------------------------------------------------------------

data("string_data")

dat1 <- string_data %>%
  filter(VARIABLE == "Temperature") %>%
  select(-SENSOR, -DEPLOYMENT_PERIOD, -VARIABLE) %>%
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


superchill_threshold <- -0.7

rm(dat1, dat2)

# Case 1: Group by DEPTH --------------------------------------------------
# p <- ggplot_variables_at_depth(dat1) +
#   geom_hline(yintercept = superchill_threshold, col = "red")

superchill <- dat1_shuffle %>%
  mutate(YEAR = year(TIMESTAMP)) %>%
  group_by(DEPTH) %>%
  arrange(TIMESTAMP, .by_group = TRUE) %>%
  mutate(CROSS_THRESH = if_else(
    lag(VALUE) > superchill_threshold &
      VALUE <= superchill_threshold, TRUE, FALSE )
  ) %>%
  filter(CROSS_THRESH) %>%
  summarise(FIRST_CHILL = min(TIMESTAMP)) %>%
  mutate(FIRST_CHILL = FIRST_CHILL - minutes(1)) %>%
  ungroup()

superchill_foo <- identify_first_superchill(
  dat1_shuffle,
  DEPTH,
  superchill_threshold = superchill_threshold
)

test_that("function identifies first superchill dates when grouped by DEPTH",{

  expect_equal(superchill, superchill_foo)

})

# Case 2: Group by YEAR and DEPTH  --------------------------------------------------
superchill <- dat %>%
  mutate(YEAR = year(TIMESTAMP)) %>%
  group_by(YEAR, DEPTH) %>%
  arrange(TIMESTAMP, .by_group = TRUE) %>%
  mutate(CROSS_THRESH = if_else(
    lag(VALUE) > superchill_threshold &
      VALUE <= superchill_threshold, TRUE, FALSE )
  ) %>%
  filter(CROSS_THRESH) %>%
  summarise(FIRST_CHILL = min(TIMESTAMP)) %>%
  mutate(FIRST_CHILL = FIRST_CHILL - minutes(1)) %>%
  ungroup()


superchill_foo <- identify_first_superchill(
  dat,
  YEAR, DEPTH,
  superchill_threshold = superchill_threshold
)

test_that("function identifies dates when grouped by YEAR and DEPTH",{

  expect_equal(superchill, superchill_foo)

})

