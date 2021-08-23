## code to prepare `string_data` dataset goes here

# libraries
library(data.table)
library(dplyr)
library(lubridate)

dat <- fread("data-raw/TGC_temperature_data.csv")

# filter to reduce file size
string_data <- dat %>%
  select(STATION, TIMESTAMP, DEPTH, VALUE) %>%
  filter(

    !(STATION == "Birchy Head" & DEPTH >= 20),
    !(STATION == "Birchy Head" &
        TIMESTAMP >= as_datetime("2020-01-25") & DEPTH != 5),

    !(STATION == "Madeline Point" & DEPTH > 5),
    !(STATION == "Madeline Point" & TIMESTAMP >= as_datetime("2020-05-01")),

    !(STATION == "Rook Island" & DEPTH == 25 | DEPTH == 2),
    !(STATION == "Rook Island" & TIMESTAMP >= as_datetime("2019-04-01")),

    row_number() %% 5 == 0

  )

usethis::use_data(string_data, overwrite = TRUE)


