# Baltimore arrests 
# Andrew <andrew.flowers@fivethirtyeight.com>

setwd("~/data-analysis/baltimore-arrests")

require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)

# Import data
arrests <- read_csv("BPD_Arrests.csv")

arrests$ArrestDate <- as.Date(arrests$ArrestDate, "%m/%d/%Y")

arrests_byDay <- arrests %>% group_by(ArrestDate) %>% summarize(total_arrests=n())

ggplot(data=arrests_byDay, aes(x=ArrestDate, y=total_arrests))+geom_line()+geom_smooth()+
  ggtitle("Arrests by Baltimore Police Department, from 1/1/2013 through 6/6/2015")

crime <- read_csv("BPD_Part_1_Victim_Based_Crime_Data.csv")

crime$CrimeDate <- as.Date(crime$CrimeDate, "%m/%d/%y")

crime_byDay <- crime %>% group_by(CrimeDate) %>% summarize(total_crimes=n())

ggplot(data=crime_byDay, aes(x=CrimeDate, y=total_crimes))+geom_line()+geom_smooth()+
  ggtitle("Total Crimes (source: Baltimore Police Department) from 1/1/2010 through 6/6/2015")



