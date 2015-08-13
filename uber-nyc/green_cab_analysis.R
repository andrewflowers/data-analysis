# Green cab analysis

setwd("~/data-analysis/uber-nyc/")

require(readr)
require(dplyr)
require(stringr)
require(lubridate)

# Aggregate green cab data into one database
green_apr15 <- read_csv('https://storage.googleapis.com/tlc-trip-data/2015/green_tripdata_2015-04.csv',
                        col_types="cccinnnninnnnnnlninii")
green_may15 <- read_csv('https://storage.googleapis.com/tlc-trip-data/2015/green_tripdata_2015-05.csv', 
                        col_types="cccinnnninnnnnnlninii")
green_june15 <- read_csv('https://storage.googleapis.com/tlc-trip-data/2015/green_tripdata_2015-06.csv',
                         col_types="cccinnnninnnnnnlninii")

vars2 <- c(1,5:6)

green_trips_apr_may_2015 <- rbind(green_apr15[,vars2], green_may15[,vars2], green_june15[,vars2])

names(green_trips_apr_may_2015) <- c("datetime", "lon", "lat")

dateTimeSplit <- str_split_fixed(green_trips_apr_may_2015$datetime, " ", n=2)

green_trips_apr_may_2015$date <- ymd(dateTimeSplit[,1])
green_trips_apr_may_2015$month <- month(green_trips_apr_may_2015$date)

write_csv(green_trips_apr_may_2015, "green_trips_apr_may_2015.csv")