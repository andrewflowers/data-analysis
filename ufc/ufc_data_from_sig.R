# Andrew Flowers
# andrew.flowers@fivethirtyeight.com

require(readr)
require(RODBC)
require(dplyr)
require(ggplot2)
require(stringr)
require(lubridate)
require(tidyr)
require(reshape2)

setwd("~/data-analysis/ufc/")

fights <- tbl_df(read_csv("latest_data.csv"))
names(fights) <- tolower(names(fights))
fights$start_dt <- dmy(fights$start_dt)
fights$year <- year(fights$start_dt)
fights <- fights %>% mutate(femaleFight=ifelse((fighter1_gender=="F" | fighter2_gender=="F"), TRUE, FALSE)) %>% # Code female as fights if just one fighter is female
      mutate(ending_time=hms(ending_time))

fights <- fights %>% 

fights$fight_duration  <- NA

for (i in 1:nrow(fights)){
  if(nchar(fights$ending_time[i])==3 | nchar(fights$ending_time[i])==4){
    fights$fight_duration[i] <- (as.numeric(strsplit(fights$ending_time[i], ":")[1])*60)+as.numeric(strsplit(fights$ending_time[i], ":")[2])
  } else if (nchar(fights$ending_time[i])==8){
    fights$fight_duration[i] <- (as.numeric(strsplit(fights$ending_time[i], ":")[2])*60)+as.numeric(strsplit(fights$ending_time[i], ":")[3])
  }  
}




fights$total_time2 <- sapply(strsplit(fights$total_time,":"),
                             function(x) {
                               x <- as.numeric(x)
                               (x[1]*60)+x[2]
                             }
)

((fights$rounds_fought-1)*(5*60))+strsplit(fights$ending_time, ":"),
  function(x) {
    x <- as.numeric(x)
    (x[1]*60)+x[2]
  }
)


# Female records
femRecs <- fights %>% 
  filter(femaleFight==TRUE) %>% 
  group_by(fight_winner) %>% summarize(n=n(), fightTime=median(total_time2, na.rm=TRUE)) %>% arrange(desc(n))

# Rousey fights
ronda <- "Ronda Rousey"
rouseyFights <- fights %>% filter(fighter1==ronda | fighter2==ronda)

# Clean submission data
re <- "\\(([^()]+)\\)"
fights <- fights %>% 
  mutate(submission=ifelse(
        # if statement
        str_sub(result, 0, 12)=="Submission (", 
         # true:
        gsub(re, "\\1", str_extract_all(result, re)),
         # false:
        submission
         ),
        result=ifelse(str_sub(result, 0, 12)=="Submission (", "Submission", result))

# Tally results
fightResult <- fights %>% group_by(result) %>% summarize(n=n()) %>% arrange(desc(n))

fightsByYear <- fights %>% group_by(year) %>% 
  summarize(n=n(),
            avgTime=mean(as.numeric(total_time), na.rm=T)) # NOT WORKING

# Tally submission
submissionTypes <- fights %>% filter(result=="Submission") %>% 
  group_by(submission) %>% summarize(n=n()) %>% arrange(desc(n))

armbarByYear <- fights %>% filter(fights$femaleFight==TRUE, submission=="Armbar") %>% 
  group_by(year) %>% summarize(n=n())



# TODO
# Fix time for duration of flight
# Why isn't Armbar data aggregating correctly over time?

# Ideas to explore
# Breakdown of fights by ending, plot over time
# Isolate armbar submission, see how that's changed since Rousey
# Timing of fight: shorter or longer
# FightMetric stats: 

