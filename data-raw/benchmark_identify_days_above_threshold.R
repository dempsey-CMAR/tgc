# April 21, 2021

library(tgc)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(strings)
library(microbenchmark)

# set up ------------------------------------------------------------------
thresh <- 18.5
min_exceed <- 1
n_hours <- 24

data(string_data)

dat <- string_data %>%
  filter(VARIABLE == "Temperature") %>%
  select(-SENSOR, -DEPLOYMENT_PERIOD, -VARIABLE)

# date ranges that should be filtered out 
spans_to_filter <- dat %>%
  mutate(EXCEED_THRESH = if_else(VALUE >= thresh, TRUE, FALSE)) %>%
  filter(EXCEED_THRESH) %>%
  select(-EXCEED_THRESH) %>%
  mutate(Start = TIMESTAMP,
         End = TIMESTAMP + hours(n_hours))

range <- spans_to_filter %>% select(Start, End)


microbenchmark(
  
  filtered1 <- dat %>%
    rowwise() %>%
    filter(any(TIMESTAMP >= range$Start &  TIMESTAMP <= range$End)),
  
  filtered2 <- setDT(dat)[TIMESTAMP %inrange% range]
  
)

# Unit: milliseconds
# expr      min        lq       mean    median       uq       max neval
# filtered1  757.8904 800.41270 832.720913 824.07565 848.7043 1068.7436   100
# filtered2   2.4897   2.71605   2.930968   2.82235   2.9036    9.0199   100


# # dat is already a data.table so don't need to convert
# # (but probably best to do it anyway in case it is not a data.table in future)
# filtered5 <- dat[TIMESTAMP %inrange% range]
# 



