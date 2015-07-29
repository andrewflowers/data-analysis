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
fights <- fights %>% mutate(femaleFight=ifelse((fighter1_gender=="F" | fighter2_gender=="F"), TRUE, FALSE)) %>% 
  filter(nchar(ending_time) %in% c(4, 5, 8), rounds_scheduled<=5)

fights$ending_time2 <- NA

for (i in 1:nrow(fights)){
  
  if(nchar(fights$ending_time[i]) == 4 | nchar(fights$ending_time[i]) == 5){
    fights$ending_time2[i] <- (as.numeric(unlist(strsplit(fights$ending_time[i], ":"))[1])*60)+as.numeric(unlist(strsplit(fights$ending_time[i], ":"))[2])
  } else if(nchar(fights$ending_time[i]) == 8){
    fights$ending_time2[i] <- (as.numeric(unlist(strsplit(fights$ending_time[i], ":"))[2])*60)+as.numeric(unlist(strsplit(fights$ending_time[i], ":"))[3])
  }
}
fights <- fights %>% filter(ending_time2<=300)

fights$fight_duration  <- as.numeric(fights$ending_time2)+((fights$rounds_fought-1)*(5*60))

# Female records
femRecs <- fights %>% 
  filter(femaleFight==TRUE) %>% 
  group_by(fight_winner) %>% summarize(n=n(), fightTime=mean(fight_duration, na.rm=TRUE)) %>% arrange(fightTime)

ggplot(data=femRecs, aes(x=n, y=fightTime))+geom_point()


# Rousey fights
ronda <- "Ronda Rousey"
rouseyFights <- fights %>% filter(fighter1==ronda | fighter2==ronda)

# All fighters
allFighters <- unite(fights, fighter, fighter1:fighter2)
fightTiming <- fights %>% 
  group_by(fight_winner) %>% summarize(wins=n(), fightTime=mean(fight_duration, na.rm=TRUE)) %>% 
  filter(wins>1) %>% arrange(fightTime)

f1 <- unique(fights$fighter1); f2 <- unique(fights$fighter2); allFighters <- rbind(f1, f2)

############# Submission Analysis #############

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



#### SCRAP HEAP ####

#          (as.numeric(unlist(strsplit(ending_time, ":"))[1])*60)+as.numeric(unlist(strsplit(ending_time, ":"))[2]),
#          ifelse(nchar(ending_time)==8, 
#                 (as.numeric(unlist(strsplit(ending_time, ":"))[2])*60)+as.numeric(unlist(strsplit(ending_time, ":"))[3]),
#                 ending_time)))
#   
#   
# }
#   mutate(ending_time=ifelse(nchar(ending_time)==4 | nchar(ending_time)==5, 
#                             (as.numeric(unlist(strsplit(ending_time, ":"))[1])*60)+as.numeric(unlist(strsplit(ending_time, ":"))[2]),
#                             ifelse(nchar(ending_time)==8, 
#                                    (as.numeric(unlist(strsplit(ending_time, ":"))[2])*60)+as.numeric(unlist(strsplit(ending_time, ":"))[3]),   
#          ifelse(nchar(ending_time)==8, hms(ending_time),
#         ending_time))) %>% filter(nchar(ending_time)>0 & nchar(ending_time)<3, rounds_fought> 0)