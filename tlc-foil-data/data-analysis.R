# TLC FOIL data analysis

# Andrew Flowers

setwd("~/data-analysis/tlc-foil-data/")

require(readr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(lubridate)
require(RgoogleMaps)
require(ggmap)

uberData <- read_csv("uber-raw-data.csv")

bases <- data.frame(base_name=c('Unter', 'Hinter', 'Weiter', 'Schmecken', 'Danach-NY'),
                    base_code=c('B02512', 'B02598', 'B02617', 'B02682', 'B02764'))

uberData$base_name <- bases[match(uberData$Base, bases$base_code),]$base_name

dateTimeSplit <- str_split_fixed(uberData$`Date/Time`, " ", n=2)

uberData$date <- mdy(dateTimeSplit[,1])

uberData$time <- hms(dateTimeSplit[,2])

uberData <- uberData %>% select(date, time, base_name, Lat, Lon)

byDay <- uberData %>% group_by(date) %>% summarize(total_rides=n())

ggplot(data=byDay, aes(x=date, y=total_rides))+geom_line()+
  geom_smooth()+
  ggtitle("Uber Pickups in NYC \n Apr. 1 to Sep. 30, 2014")+
  xlab("Day of Pickup")+ylab("Uber Pickups")

# Privacy test
officeRides <- uberData %>% filter(Lat==40.77395, Lon==-73.98060)
officeRides <- uberData %>% filter(Lat==40.7, Lon==-73.9)

######### Begin mapping data
uberData_small <- uberData[1:1000,]
map <- get_map(location=c(lon=mean(uberData$Lon), lat=mean(uberData$Lat)), zoom=12, scale=2,
               maptype="roadmap")

nycMap <- ggmap(map)+geom_point(data=uberData, aes(x=Lon, y=Lat, fill="red", alpha=0.8), size=2, shape=21)+
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

nycMap

ggsave(nycMap, "nyc_uber_map.png")

geocode("new york city")





