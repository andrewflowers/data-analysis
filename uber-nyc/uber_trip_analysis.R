# Uber trip analysis
# Andrew Flowers

setwd("~/data-analysis/uber-nyc/")

require(readr)
require(stringr)
require(lubridate)
require(dplyr)
require(ggplot2)

##############################
## Load and clean Uber data ##
##############################

rawUberTrips <- read_csv("raw-tlc-data/uber-raw-data.csv")

bases <- data.frame(base_name=c('Unter', 'Hinter', 'Weiter', 'Schmecken', 'Danach-NY'),
                    base_code=c('B02512', 'B02598', 'B02617', 'B02682', 'B02764'))

rawUberTrips$base_name <- bases[match(rawUberTrips$Base, bases$base_code),]$base_name

dateTimeSplit <- str_split_fixed(rawUberTrips$`Date/Time`, " ", n=2)

rawUberTrips$date <- mdy(dateTimeSplit[,1])

rawUberTrips$time <- hms(dateTimeSplit[,2])

cleanUberTrips <- rawUberTrips %>% select(date, time, base_name, Lat, Lon)

#######################
## Analyze Uber data ##
#######################

byDay <- cleanUberTrips %>% group_by(date) %>% summarize(total_rides=n())

ggplot(data=byDay, aes(x=date, y=total_rides))+geom_line()+
  geom_smooth()+
  ggtitle("Uber Pickups in NYC \n Apr. 1 to Sep. 30, 2014")+
  xlab("Day of Pickup")+ylab("Uber Pickups")

## Uber time/day of week analysis

cleanUberTrips$day <- as.numeric(wday(cleanUberTrips$date))
cleanUberTrips$hour <- as.numeric(hour(cleanUberTrips$time))
table(cleanUberTrips$day)/length(cleanUberTrips$day)
table(cleanUberTrips$hour)/length(cleanUberTrips$hour)

cleanUberTrips$month <- month(cleanUberTrips$date)

byMonth <- cleanUberTrips %>% group_by(month) %>% summarize(total_rides=n())

ggplot(data=byMonth, aes(x=month, y=total_rides))+geom_line()+
  ggtitle("Monthly Uber Pickups in NYC \n Apr. to Sep. 2014")+
  xlab("Month of Pickup")+ylab("Uber Pickups")
