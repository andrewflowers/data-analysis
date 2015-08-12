# Uber and taxi trip analysis by geography
# Andrew Flowers

setwd("~/data-analysis/uber-nyc/")

require(readr)
require(stringr)
require(lubridate)
require(dplyr)
require(ggplot2)
require(reshape2)

# Boro codes
boroCodes <- data.frame(codes=c(5, 47, 61, 81, 85),
                        name=c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))

## Aggregate trip data (Apr-Sept)
agg_trips <- read_csv("for_analysisv2.csv")
carl_coding <- read_csv("carl_manhattan_coding.csv")

# Clean names
names(agg_trips) <- tolower(names(agg_trips))
names(agg_trips)[8:10] <- c("uber_total", "green_total", "yellow_total")
agg_trips$cab_total <- agg_trips$green_total + agg_trips$yellow_total

agg_trips$boro <- boroCodes[match(agg_trips$countyfp, boroCodes$codes),]$name

agg_trips$cbd <- carl_coding[match(agg_trips$geoid, carl_coding$GEOID),]$CBD

length(unique(agg_trips$geoid)) # 2165 unique geoids  

agg_trips <- agg_trips %>% 
  mutate(airport=ifelse(tractce %in% c(71600, 33100) & boro=="Queens", TRUE, FALSE))

table(agg_trips$boro)

# Trip data for Apr and Sept 2014 separately
apr_sept_trips <- data.frame()

# Six csv files storied in sub-directory `/geo-trip data` include:
# "green_data_april2014.csv" "green_data_sept2014.csv"  "taxi_data_april2014.csv" 
# "taxi_data_sept2014.csv"   "uber_data_april2014.csv"  "uber_data_sept2014.csv"  
list.files("./geo-trip-data/")

# Aggregate those six files into a master file of the Apr and Sept trips by geoid
for (f in list.files("./geo-trip-data/")){
  temp <- read_csv(paste0("./geo-trip-data/",f))
  temp$old_name <- f
  apr_sept_trips <- rbind(apr_sept_trips, temp)
}

length(unique((apr_sept_trips$geoid))) # 2923 unique geoids 

fileNames <- data.frame(old_name=unique(apr_sept_trips$old_name),
                        new_name=c("green_apr", "green_sept",
                                   "yellow_apr", "yellow_sept",
                                   "uber_apr", "uber_sept"))

apr_sept_trips$file <- fileNames[match(apr_sept_trips$old_name, fileNames$old_name),]$new_name

table(apr_sept_trips$file, useNA = "ifany") 

length(common_tracts <- intersect(agg_trips$geoid, apr_sept_trips$geoid)) # 2160 common geoids between two data sets

apr_sept_trips$boro <- agg_trips[match(apr_sept_trips$geoid, agg_trips$geoid),]$boro

table(apr_sept_trips$boro, useNA = "ifany") 

length(common_tracts <- intersect(agg_trips$geoid, apr_sept_trips[!is.na(apr_sept_trips$boro),]$geoid))

# Investigate apr_sept_trips where no boro is found
# Presumably geoid is outside of NYC

# View(apr_sept_trips %>% filter(is.na(boro))) # Might want to investigate where these geoids are exactly -- outside NYC, right?

apr_sept_trips_nyc <- apr_sept_trips %>% filter(!is.na(boro))

apr_sept_trips_cast <- dcast(data=apr_sept_trips_nyc, 
                            formula=geoid~file, 
                            value.var="total")

# Create `all_trips` data frame includes data by 2160 geoids (census tracts) in NYC

all_trips <- agg_trips %>%  right_join(apr_sept_trips_cast, by="geoid")
all_trips[is.na(all_trips)] <- 0

agg_byBoro <- all_trips %>% group_by(boro) %>% 
  summarize(uber_total=sum(uber_total, na.rm=T),
            green_total=sum(green_total, na.rm=T),
            yellow_total=sum(yellow_total, na.rm=T),
            taxi_total=green_total+yellow_total,
            total_trips=uber_total+taxi_total)

# 22% of Uber pickups are outside of Manhattan
sum(agg_byBoro[agg_byBoro$boro!="Manhattan",]$uber_total)/sum(agg_byBoro$uber_total)

# 14% of taxi pickups are outside of Manhattan
sum(agg_byBoro[agg_byBoro$boro!="Manhattan",]$taxi_total, na.rm=T)/sum(agg_byBoro$taxi_total, na.rm=T)

# 4.5% of Uber pickups are from airports
sum(all_trips[all_trips$airport==TRUE,]$uber_total, na.rm=T)/sum(all_trips$uber_total, na.rm=T)

# 3.8% of taxi pickups are from airports
sum(all_trips[all_trips$airport==TRUE,]$cab_total, na.rm=T)/sum(all_trips$cab_total, na.rm=T)

# 63% of Uber trips are in the Central Business District (CBD)
sum(all_trips[all_trips$cbd==1,]$uber_total, na.rm=T)/sum(all_trips$uber_total, na.rm=T)

# 62% of taxi trips are in the Central Business District (CBD)
sum(all_trips[all_trips$cbd==1,]$cab_total, na.rm=T)/sum(all_trips$cab_total, na.rm=T)

apr_sept_byBoro <- all_trips %>% group_by(boro) %>% 
  summarize(uber_apr=sum(uber_apr, na.rm=T),
            uber_sept=sum(uber_sept, na.rm=T),
            green_apr=sum(green_apr, na.rm=T),
            green_sept=sum(green_sept, na.rm=T),
            yellow_apr=sum(yellow_apr, na.rm=T),
            yellow_sept=sum(yellow_sept, na.rm=T),
            uber_chng=uber_sept-uber_apr,
            green_chng=green_sept-green_apr,
            yellow_chng=yellow_sept-yellow_apr)

# Decline in Manhattan Yellow/Green cab trips was 3.79x the increase in Uber trips between Apr and Sept 2014
(2272+1159075)/306657

# Analyze Manhattan census tracts where Uber strongest, and see if Yellow cab declined
# 288 Manhattan census tracts --> filter to 211 that had at least 1000 total Uber trips

manhattan <- all_trips %>% filter(boro=="Manhattan", uber_total>=100) %>% 
  mutate(uber_chng_pct=((uber_sept)/(uber_apr)-1)*100,
         taxi_chng_pct=((yellow_sept+green_sept)/(yellow_apr+green_apr)-1)*100)

g1 <- ggplot(data=manhattan, aes(x=uber_chng_pct, y=taxi_chng_pct))+
  geom_point()+geom_smooth()+
  xlab("% Change in Uber Trips")+ylab("% Change in Yellow/Green Taxi Trips")+
  ggtitle("Uber vs. Taxis\nManhattan census tracts with minimum of 1,000 Uber trips\nApr-Sept 2014")
g1
ggsave("uber_vs_taxis_manhattan.png")

g2 <- ggplot(data=manhattan, aes(x=uber_chng_pct, y=taxi_chng_pct, size=uber_total))+
  geom_point()+geom_smooth()+
  xlab("% Change in Uber Trips")+ylab("% Change in Yellow/Green Taxi Trips")+
  ggtitle("Uber vs. Taxis\nManhattan census tracts with minimum of 1,000 Uber trips\nApr-Sept 2014")
g2
ggsave("uber_vs_taxis_manhattan_sized.png")



## Analyze by race, age, language, etc.
censusData <- read_csv("census_data_by_nyc_tract.csv")
test <- cbind(all_trips, censusData[match(all_trips$geoid, censusData$geoid),])

