# TLC FOIL data analysis
# Andrew Flowers

setwd("~/data-analysis/tlc-foil-data/")

require(readr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(lubridate)

uberData <- read_csv("uber-raw-data.csv")

bases <- data.frame(base_name=c('Unter', 'Hinter', 'Weiter', 'Schmecken', 'Danach-NY'),
                    base_code=c('B02512', 'B02598', 'B02617', 'B02682', 'B02764'))

uberData$base_name <- bases[match(uberData$Base, bases$base_code),]$base_name

dateTimeSplit <- str_split_fixed(uberData$`Date/Time`, " ", n=2)

uberData$date <- dmy(dateTimeSplit[,1])

uberData$time <- hms(dateTimeSplit[,2])

uberData <- uberData %>% select(date, time, base_name, Lat, Lon)
