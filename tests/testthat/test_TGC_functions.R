# November 15, 2021

# test TGC functions

library(ddpcr) # to suppress print from error message
library(dplyr)
library(tgc)


# Set up ------------------------------------------------------------------

#source(system.file("testdata/test_data.R", package = "tgc"))

dd <- tibble(STATION = c("Station1", "Station2"),
             n_degree_days = c(100, 200))


tgc = c(0.25, 0.3, 0.35)
final_weight = c(5, 5.5)
initial_weight = c(0.5)


TGC_initial_weight <- TGC_calculate_initial_weight(dd, final_weight, tgc)

TGC_final_weight <- TGC_calculate_final_weight(dd, initial_weight, tgc)

TGC_dd <- TGC_calculate_degree_days(initial_weight, final_weight, tgc)


# Number of rows ----------------------------------------------------------

test_that("TGC functions have correct number of rows", {

  expect_equal(nrow(TGC_initial_weight),
               nrow(dd) * length(final_weight) * length(tgc))

  expect_equal(nrow(TGC_final_weight),
               nrow(dd) * length(initial_weight) * length(tgc))

  expect_equal(nrow(TGC_dd),
               length(initial_weight) * length(final_weight) * length(tgc))

})



# Check results -----------------------------------------------------------

test_that("TGC_calculate_degree_days() will stop with error if final_weight is less than intial_weight",{

  expect_error(
    ddpcr:quiet(TGC_calculate_degree_days(initial_weight = 5, final_weight = 4, tgc = 0.3))
  )

})

# set up values to compare
initial_check <- filter(TGC_final_weight, n_degree_days == 100, TGC == 0.3)

initial <- TGC_calculate_initial_weight(
  dd = tibble(n_degree_days = 100),
  final_weight = initial_check$TGC_FINAL_WEIGHT,
  tgc = 0.3
)


final_check <- filter(TGC_initial_weight, n_degree_days == 100, TGC == 0.3)

final <- TGC_calculate_final_weight(
  dd = tibble(n_degree_days = 100),
  initial_weight = final_check$TGC_INITIAL_WEIGHT,
  tgc = 0.3
)


dd_check <- TGC_calculate_degree_days(initial_weight = final_check$TGC_INITIAL_WEIGHT,
                          final_weight =  final_check$FINAL_WEIGHT,
                          tgc = 0.3) %>%
  filter(TGC_DEGREE_DAYS < 200)


test_that("TGC functions output expected results", {

  expect_equal(initial_check$INITIAL_WEIGHT, initial$TGC_INITIAL_WEIGHT)

  expect_equal(final_check$FINAL_WEIGHT, final$TGC_FINAL_WEIGHT)

  expect_equal(round(dd_check$TGC_DEGREE_DAYS), c(100, 100))

})


