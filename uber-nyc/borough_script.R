# Uber/taxi data analysis
# Andrew Flowers

setwd("~/data-analysis/tlc-foil-data/")
source("~/data-analysis/Five38Thm.R")

require(readr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(lubridate)
require(RgoogleMaps)
require(ggmap)

rideData <- read_csv("for_analysisv2.csv")
tractData <- read_csv("nyc_tract_info.csv")
aprData <- read_csv("uber_data_april2014.csv")
septData <- read_csv("uber_data_sept2014.csv")
aprData_yel <- read_csv("taxi_data_april2014.csv")
septData_yel <- read_csv("taxi_data_sept2014.csv")
aprData_gre <- read_csv("green_data_april2014.csv")
septData_gre <- read_csv("green_data_sept2014.csv")

rideData$total_cab_rides <- rideData$green_data_green_data + rideData$yellow_data_yellow_count
rideData$all_rides <- rideData$total_cab_rides + rideData$uber_data_uber_count

nycCodes <- data.frame(codes=c(5, 47, 61, 81, 85),
                       name=c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))

rideData$borough <- nycCodes[match(rideData$COUNTYFP, nycCodes$codes),]$name

rideData$apr_Uber_rides <- as.numeric(aprData[match(rideData$GEOID, aprData$geoid),]$total)
rideData$sept_Uber_rides <- as.numeric(septData[match(rideData$GEOID, septData$geoid),]$total)
rideData$chng_Uber_rides <- as.numeric(((rideData$sept_Uber_rides / rideData$apr_Uber_rides)-1)*100)

rideData$apr_yel_rides <- as.numeric(aprData_yel[match(rideData$GEOID, aprData_yel$geoid),]$total)
rideData$sept_yel_rides <- as.numeric(septData_yel[match(rideData$GEOID, septData_yel$geoid),]$total)
rideData$apr_gre_rides <- as.numeric(aprData_gre[match(rideData$GEOID, aprData_gre$geoid),]$total)
rideData$sept_gre_rides <- as.numeric(septData_gre[match(rideData$GEOID, septData_gre$geoid),]$total)

rideData$apr_cab_rides <- rideData$apr_yel_rides + rideData$apr_gre_rides
rideData$sept_cab_rides <- rideData$sept_yel_rides + rideData$sept_gre_rides
rideData$chng_cab_rides <- as.numeric(((rideData$sept_cab_rides / rideData$apr_cab_rides)-1)*100)

rideData$apr_Uber_share <- rideData$apr_Uber_rides /(rideData$apr_Uber_rides + rideData$apr_cab_rides)
rideData$sept_Uber_share <- rideData$sept_Uber_rides /(rideData$sept_Uber_rides + rideData$sept_cab_rides)
rideData$chng_Uber_share <- as.numeric(rideData$sept_Uber_share - rideData$apr_Uber_share)

byBorough <- rideData  %>% group_by(borough) %>% 
  summarize(apr_cab_rides=sum(apr_cab_rides, na.rm = T),
            sept_cab_rides=sum(sept_cab_rides, na.rm = T))

sum(byBorough$sept_cab_rides)-sum(byBorough$apr_cab_rides)