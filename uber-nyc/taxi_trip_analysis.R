# Taxi trip analysis
# Andrew Flowers

setwd("~/data-analysis/uber-nyc/")

require(readr)
require(stringr)
require(lubridate)
require(dplyr)
require(ggplot2)

###########################################
## Yellow and Green Cab Trip Record Data ##
###########################################

## To download directly from TLC website: http://www.nyc.gov/html/tlc/html/about/statistics.shtml
# apr2014yel <- read_csv("https://storage.googleapis.com/tlc-trip-data/2014/yellow_tripdata_2014-04.csv")

## Read for doownloaded csv files
# apr2014 <- read_csv("taxi-trip-data/yellow_tripdata_2014-04.csv", col_types="cccinnnicnncnnnnnn")
# may2014 <- read_csv("taxi-trip-data/yellow_tripdata_2014-05.csv", col_types="cccinnnicnncnnnnnn")
# jun2014 <- read_csv("taxi-trip-data/yellow_tripdata_2014-06.csv", col_types="cccinnnicnncnnnnnn")
# jul2014 <- read_csv("taxi-trip-data/yellow_tripdata_2014-07.csv", col_types="cccinnnicnncnnnnnn")
# aug2014 <- read_csv("taxi-trip-data/yellow_tripdata_2014-08.csv", col_types="cccinnnicnncnnnnnn")
# sep2014 <- read_csv("taxi-trip-data/yellow_tripdata_2014-09.csv", col_types="cccinnnicnncnnnnnn")

vars <- c(2,6:7)

yellow <- rbind(apr2014[,vars], may2014[,vars], jun2014[,vars], jul2014[,vars], aug2014[,vars], sep2014[,vars])

names(yellow) <- c("datetime", "lon", "lat")

dateTimeSplit <- str_split_fixed(yellow$datetime, " ", n=2)

yellow$date <- ymd(dateTimeSplit[,1])
yellow$month <- month(yellow$date)

byDay <- yellow %>% group_by(date) %>% summarize(n=n())
byMonth <- yellow %>% group_by(month) %>% summarize(n=n())

#yellow$time <- hms(dateTimeSplit[,2])

#yellow$type <- 'yellow'
# 
#yellow2 <- yellow %>% select(date, time, lon, lat, type)

#write_csv(yellow2, "clean-yellow-cab-rides.csv")
# 
# yellow$day <- as.numeric(wday(yellow$date))
# yellow$hour <- as.numeric(hour(yellow$time))
# table(yellow$day)
# table(yellow$hour)

### Greeen ###

green_apr2014 <- read_csv("green/green_tripdata_2014-04.csv", col_types="cccinnnninnnnnnlninii")
green_may2014 <- read_csv("green/green_tripdata_2014-05.csv", col_types="cccinnnninnnnnnlninii")
green_jun2014 <- read_csv("green/green_tripdata_2014-06.csv", col_types="cccinnnninnnnnnlninii")
green_jul2014 <- read_csv("green/green_tripdata_2014-07.csv", col_types="cccinnnninnnnnnlninii")
green_aug2014 <- read_csv("green/green_tripdata_2014-08.csv", col_types="cccinnnninnnnnnlninii")
green_sep2014 <- read_csv("green/green_tripdata_2014-09.csv", col_types="cccinnnninnnnnnlninii")

vars2 <- c(1,5:6)

green <- rbind(green_apr2014[,vars], green_may2014[,vars], green_jun2014[,vars], 
               green_jul2014[,vars], green_aug2014[,vars], green_sep2014[,vars])

names(green) <- c("datetime", "lon", "lat")

green$type <- 'green'

dateTimeSplit <- str_split_fixed(green$datetime, " ", n=2)

green$date <- ymd(dateTimeSplit[,1])
green$month <- month(green$date)

byDay_gre <- green %>% group_by(date) %>% summarize(n=n())
byMonth_gre <- green %>% group_by(month) %>% summarize(n=n())

ggplot(data=byDay_gre, aes(x=date, n))+geom_point()+geom_smooth()

green$time <- hms(dateTimeSplit[,2])

green <- green %>% select(date, time, lon, lat, type)

green$day <- as.numeric(wday(green$date))
green$hour <- as.numeric(hour(green$time))
table(green$day)
table(green$hour)
