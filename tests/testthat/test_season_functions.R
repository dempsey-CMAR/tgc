library(tgc)
library(dplyr)

# Set up ------------------------------------------------------------------

data("string_data")

results <- readRDS(system.file("testdata/test_results.rds", package = "tgc"))

# shuffle to make sure order does not matter
set.seed(468)
rows <- sample(nrow(string_data))
string_data <- string_data[rows, ]

#  identify_trending_up() --------------------------------------------------

trend_up <- identify_trending_up(string_data, STATION)

test_that("identify_trending_up() outputs expected results",{

  expect_equal(trend_up, results$trend_up)

})


# identify_first_superchill() ---------------------------------------------

superchill <- identify_first_superchill(string_data, STATION)

test_that("identify_first_superchill() outputs expected results",{

  expect_equal(superchill, results$superchill)

})


# identify_growing_seasons() ---------------------------------------------

seasons <- identify_growing_seasons(string_data, STATION) %>%
  arrange(STATION, SEASON, DEPTH)


seasons_partial <- string_data %>%
  identify_growing_seasons(STATION, full_season = FALSE) %>%
  arrange(STATION, SEASON, DEPTH)


test_that("identify_growing_seasons() outputs expected results",{

  expect_equal(nrow(seasons), nrow(seasons_partial))

  expect_equal(na.omit(seasons), results$seasons, ignore_attr = TRUE)
  expect_equal(seasons_partial, results$seasons_partial)

})


