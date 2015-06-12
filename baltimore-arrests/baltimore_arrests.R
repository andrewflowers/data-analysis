# Baltimore arrests 
# Andrew <andrew.flowers@fivethirtyeight.com>

setwd("~/data-analysis/baltimore-arrests")

require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)
require(reshape2)
require(zoo)

# Arrests
arrests <- read_csv("BPD_Arrests.csv")

arrests$ArrestDate <- as.Date(arrests$ArrestDate, "%m/%d/%Y")

arrests_byDay <- arrests %>% group_by(ArrestDate) %>% summarize(total_arrests=n())

ggplot(data=arrests_byDay, aes(x=ArrestDate, y=total_arrests))+geom_line()+geom_smooth()+
  ggtitle("Arrests by Baltimore Police Department, from 1/1/2013 through 6/6/2015")

# Crime
crime <- read_csv("BPD_Part_1_Victim_Based_Crime_Data.csv")

crime$CrimeDate <- as.Date(crime$CrimeDate, "%m/%d/%y")

crime_byDay <- crime %>% group_by(CrimeDate) %>% summarize(total_crimes=n()) %>% arrange(desc(CrimeDate))

ggplot(data=crime_byDay, aes(x=CrimeDate, y=total_crimes))+geom_line()+geom_smooth()+
  ggtitle("Total Crimes (source: Baltimore Police Department) from 1/1/2010 through 6/6/2015")

crime_byDay <- crime_byDay %>% 
  mutate(year_ago = lead(total_crimes, 365), year_change = ((total_crimes - year_ago)/year_ago)*100) %>% 
  filter(CrimeDate>="2014-01-01")

ggplot(data=crime_byDay, aes(x=CrimeDate, y=year_change))+geom_line()+geom_smooth()+
  ggtitle("Crime: Year-over-year % Change \n(source: Baltimore Police Department) from 1/1/2014 through 6/6/2015")

crimeType <- dcast(crime, formula=CrimeDate~Description)
names(crimeType) <- tolower(names(crimeType))
crimeType <- crimeType %>% mutate(totalRob=`robbery - carjacking`+ `robbery - commercial`+ `robbery - residence` + `robbery - street`)

crimeType_week <- tbl_df(data.frame(crimedate=crimeType$crimedate[1:1977],
                             homicide=rollsum(crimeType$homicide, 7),
                             auto_theft=rollsum(crimeType$`auto theft`, 7),
                             robbery=rollsum(crimeType$totalRob, 7),
                             shootings=rollsum(crimeType$shooting, 7)
                             )) %>% filter(crimedate>="2011-01-01") %>% arrange(desc(crimedate))

crimeType_week2 <- crimeType_week %>% 
  mutate(homicide_lag=homicide - lead(homicide, 365),
        auto_theft_lag=auto_theft - lead(auto_theft, 365),
        robbery_lag=robbery - lead(robbery, 365),
        shootings_lag=shootings - lead(shootings, 365)) %>% 
  filter(crimedate>="2012-01-01") %>% select(1:5)

ggplot(data=crimeType_week, aes(x=crimedate, y=homicide))+geom_point()+geom_smooth()+
  ggtitle("Year-over-year change in Homicides in Baltimore, 7-day rolling sum")

ggplot(data=crimeType_week, aes(x=crimedate, y=auto_theft))+geom_point()+geom_smooth()+
  ggtitle("Year-over-year change in Auto Thefts in Baltimore, 7-day rolling sum")

ggplot(data=crimeType_week, aes(x=crimedate, y=robbery_lag))+geom_point()+geom_smooth()+
  ggtitle("Year-over-year change in robberies in Baltimore, 7-day rolling sum")

ggplot(data=crimeType_week, aes(x=crimedate, y=shootings_lag))+geom_point()+geom_smooth()+
  ggtitle("Year-over-year change in shootings in Baltimore, 7-day rolling sum")

crimeType2 <- crimeType_week2 %>% gather(crime, average, -crimedate)

ggplot(data=crimeType2, aes(x=crimedate, average))+geom_point()+geom_smooth()+facet_grid(. ~ crime)




