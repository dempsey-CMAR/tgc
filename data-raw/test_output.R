library(tgc)
library(dplyr)

data("string_data")

# note: this is picking up oscillations as the temperature decreases
# in the winter. identify_growing_seasons() only looks from Jan - Aug to
# avoid this
# consider building the months filter in here
trend_up <- string_data %>%
  identify_trending_up(STATION) %>%
  arrange(STATION, DEPTH)

# note: this is picking up the first superchill in the time series
superchill <- string_data %>%
  identify_first_superchill(STATION) %>%
  arrange(STATION, DEPTH)

#
seasons <- string_data %>%
  identify_growing_seasons(STATION) %>%
  na.omit() %>%
  arrange(STATION, SEASON, DEPTH)

seasons_partial <- string_data %>%
  identify_growing_seasons(STATION, full_season = FALSE) %>%
  arrange(STATION, SEASON, DEPTH)

test_results = list(trend_up = trend_up,
                    superchill = superchill,
                    seasons = seasons,
                    seasons_partial = seasons_partial)

saveRDS(test_results, "inst/testdata/test_results.rds")





