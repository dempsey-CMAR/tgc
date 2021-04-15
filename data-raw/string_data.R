## code to prepare `srting_data` dataset goes here

# libraries
library(strings) # for string data functions

data(tidy_data)

string_data <- tidy_data

usethis::use_data(string_data, overwrite = TRUE)


