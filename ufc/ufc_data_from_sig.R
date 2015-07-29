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

source("~/data-analysis/Five38Thm.R")

setwd("~/data-analysis/ufc/")

fights <- tbl_df(read_csv("latest_data2.csv"))
names(fights) <- tolower(names(fights))
fights$start_dt <- dmy(fights$start_dt)
fights$year <- year(fights$start_dt)
fights <- fights %>% mutate(femaleFight=ifelse((fighter1_gender=="F" | fighter2_gender=="F"), TRUE, FALSE),
                            fighter1_gender=ifelse(femaleFight, "F", "M"),
                            fighter2_gender=ifelse(femaleFight, "F", "M")) %>% 
  filter(nchar(ending_time) %in% c(4, 5, 8), rounds_scheduled<=5, rounds_scheduled>0)

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
fights$fight_duration2 <- fights$fight_duration/(as.numeric(fights$rounds_scheduled)*(5*60))

advStats <- fights %>% filter(!is.na(strike_accuracy))

# Add fight loser variable
fights$fight_loser <- ifelse(fights$fighter1==fights$fight_winner, fights$fighter2, fights$fighter1)

# Select original filters
#fights <- fights %>% filter(femaleFight==TRUE)

# Tally wins and losses
fightWinners <- fights %>% 
  group_by(fight_winner) %>% summarize(wins=n()) %>% arrange(desc(wins))
fightLosers <- fights %>% 
  group_by(fight_loser) %>% summarize(losses=n()) %>% arrange(desc(losses))

losers <- data.frame(fighter=unique(fightLosers$fight_loser))
winners <- data.frame(fighter=unique(fightWinners$fight_winner))
allFighters <- unique(rbind(winners, losers))

allFighters$wins <- fightWinners[match(allFighters$fighter, fightWinners$fight_winner),]$wins
allFighters$losses <- fightLosers[match(allFighters$fighter, fightLosers$fight_loser),]$losses

# Convert NAs to 0
allFighters[is.na(allFighters)] <- 0
allFighters$winPct <- (allFighters$wins/(allFighters$wins+allFighters$losses))*100  

# Add average time in wins/losses
fightTiming_wins <- fights %>% group_by(fight_winner) %>% summarize(fightTime=mean(fight_duration, na.rm=TRUE), fightTime2=(1-mean(fight_duration2, na.rm=TRUE)))
fightTiming_losses <- fights %>% group_by(fight_loser) %>% summarize(fightTime=mean(fight_duration, na.rm=TRUE), fightTime2=(1-mean(fight_duration2, na.rm=TRUE)))

allFighters$avgWinTime <- fightTiming_wins[match(allFighters$fighter, fightTiming_wins$fight_winner),]$fightTime
allFighters$avgWinPct <- fightTiming_wins[match(allFighters$fighter, fightTiming_wins$fight_winner),]$fightTime2
allFighters$avgLossTime <- fightTiming_losses[match(allFighters$fighter, fightTiming_losses$fight_loser),]$fightTime
allFighters$avgLossPct <- fightTiming_losses[match(allFighters$fighter, fightTiming_losses$fight_loser),]$fightTime2

# Convert NAs to 0
allFighters[is.na(allFighters)] <- 0

allFighters$timeScores <- (allFighters$avgWinPct*allFighters$winPct) - (allFighters$avgLossPct*(100-allFighters$winPct))


# Ronda
ronda <- "Ronda Rousey"
rouseyFights <- fights %>% filter(fighter1==ronda | fighter2==ronda)
allFighters %>% filter(fighter==ronda)

ggplot(data=allFighters, aes(x=avgWinTime, y=wins))+geom_point(color="blue")

g <- ggplot(data=subset(allFighters, wins>=3), aes(x=timeScores, y=winPct))+
  geom_point(aes(size=wins), color="blue") + geom_point(data=subset(allFighters, fighter==ronda), color="red")+
  xlab("Fight Speed Score [Weighted] \n(average % fight time remaining in wins - average % fight time remaining in losses )")+
  ylab("Win Percentage")+ggtitle("Ronda Rousey Wins A Lot -- And Quickly\n (All MMA fighters with at least 3 wins)") #+Five38Thm
g
ggsave(g, filename = "rousey_plot.png")

# Adv stats

fighterAdvStats <- advStats %>% 
  #filter(femaleFight==TRUE) %>% 
  group_by(fight_winner) %>% 
  summarize(wins=n(),
            strikePct=mean(strike_accuracy, na.rm=TRUE),
            takedownPct=mean(takedown_accuracy, na.rm=TRUE),
            strikePctW=weighted.mean(strike_accuracy, w=fight_duration, na.rm=TRUE),
            takedownPctW=weighted.mean(takedown_accuracy, w=fight_duration, na.rm=TRUE)) %>% 
  arrange(desc(strikePct))

ggplot(data=subset(fighterAdvStats), aes(x=strikePctW, takedownPctW))+geom_point()


# Female records
femRecs <- fights %>% 
  filter(femaleFight==TRUE) %>% 
  group_by(fight_winner) %>% summarize(n=n(), fightTime=mean(fight_duration, na.rm=TRUE)) %>% arrange(fightTime)

ggplot(data=femRecs, aes(x=n, y=fightTime))+geom_point()

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

fights$result <- str_trim(fights$result)

# Tally results
fightResult <- fights %>% group_by(result) %>% summarize(n=n()) %>% arrange(desc(n))

fightsByYear <- fights %>% group_by(year) %>% 
  #filter(year>1997) %>% 
  summarize(n=n(),
            avgTime=mean(fight_duration, na.rm=T))

# Categories by year
submissionsByYear <- fights %>%
  filter(result=="Submission") %>% 
  group_by(year) %>% 
  summarize(submissions=n())

fightsByYear$submission <- submissionsByYear[match(fightsByYear$year,submissionsByYear$year),]$submissions
fightsByYear$submissionShare <- (fightsByYear$submission/fightsByYear$n)*100
ggplot(fightsByYear, aes(x=year, y=submissionShare))+geom_line()

# Tally submission
submissionTypes <- fights %>% filter(result=="Submission") %>% 
  group_by(submission) %>% summarize(n=n()) %>% arrange(desc(n))

# Recategorize variables
ko_tko <- c("TKO", "KO/TKO", "KO", "TKO (Punches)", "KO (Punch)", "TKO (Doctor Stoppage)", "TKO - Doctor's Stoppage", 
        "TKO (Corner Stoppage)", "TKO (Retirement)", "TKO (Injury)", "KO (Head Kick)", "KO (Knee)", "TKO (Cut)", 
        "TKO (Knees)", "KO (Slam)", "TKO (Knee)", "TKO (Doctor's Stoppage)", "TKO - Head Kick", "TKO (Exhaustion)", 
        "TKO (Knees and Punches)", "KO (Knees and Punches)")

dec <- c("Decision - Unanimous", "Unanimous Decision", "Decision - Split", "Split Decision", 
         "Draw", "Majority Decision", "Decision - Majority", "Decision", "Decision - Majority (Draw)",
         "Majority Draw", "Decision - Split (Draw)", "Technical Decision", "Decision - Unanimous (Draw)")

other <- c("No Contest", "DQ", "Could Not Continue", "DQ (Illegal Kick)", "Win",
           "DQ (Kicking a Downed Opponent)", "DQ (Eye Gouging)", "DQ (Illegal Elbows)", "No Decision (Overturned by NSAC)",
           "None", "Other", "Match Canceled", "Overturned", "")

sub <- c("Technical Submission", "Technical Submission (Armbar)", "Technical Submission (Rear-Naked Choke)", 
         "Sumission (Arm Triangle)", "Submission Strikes")

fights <- fights %>% 
  mutate(result=ifelse(result %in% ko_tko, "ko_tko", 
                       ifelse(result %in% dec, "dec", 
                              ifelse(result %in% other, "other",
                                     ifelse(result %in% sub, "Submission",
                                            result)))))
# Tally (new) results
fightResult <- fights %>% group_by(result) %>% summarize(n=n()) %>% arrange(desc(n))

# Plot all categories over time
catByYear <- fights %>% group_by(year, result) %>% summarize(n=n()) %>% mutate(allFights=sum())
ggplot(data=catByYear, aes(x=year, y=n, color=result))+geom_line()
catByYear2 <- dcast(catByYear, formula=year~result, fun.aggregate = sum, value.var='n')
byYear <- cbind(fightsByYear[1:2], catByYear2[2:5])
byYear$dec_share <- (byYear$dec/byYear$n)*100
byYear$ko_tko_share <- (byYear$ko_tko/byYear$n)*100
byYear$other_share <- (byYear$other/byYear$n)*100
byYear$Submission_share <- (byYear$Submission/byYear$n)*100

byYear2 <- byYear %>% select(year, dec_share, ko_tko_share, Submission_share, other_share) %>%  melt(id="year")

ggplot(data=subset(byYear2, year>1997), aes(x=year, y=value, color=variable))+geom_line()+xlab("Year")+
  ylab("Percent of fights")+ggtitle("Submissions are becoming more rare; KO's are Flat; Decisions are up")+
  geom_smooth(method="lm", se=FALSE)


# Submission types over time
subFights <- fights %>% filter(result=="Submission") %>% 
  mutate(submission=ifelse(submission=="Guillotine Choke", "Guillotine",
                                              ifelse(submission=="", "Unknown", submission)))

topSub <- subFights %>% group_by(submission) %>% summarise(n=n()) %>% top_n(10)

subTypesByYear <- subFights %>% 
  filter(submission %in% topSub$submission) %>% 
  group_by(year, submission) %>% summarize(n=n())
  
ggplot(data=subTypesByYear, aes(x=year, y=n, color=submission))+geom_line()
  
subTypesByYear2 <- dcast(subTypesByYear, formula=year~submission)
subTypesByYear2$totalSub <- rowSums(subTypesByYear2[,2:11], na.rm=TRUE)
subTypesByYear3 <- cbind(subTypesByYear2$year,(subTypesByYear2[,2:11]/subTypesByYear2[,12])*100)
subTypesByYear3[is.na(subTypesByYear3)] <- 0
names(subTypesByYear3)[1] <- 'year'
subTypesPlot <- melt(subTypesByYear3, id="year")

ggplot(data=subset(subTypesPlot, year>1997), aes(x=year, y=value, color=variable))+geom_line()+
  ggtitle("Rousey's Favorite Submission (the Armbar) Is Becoming Less Effective")+ylab("Percent of Submissions")+
  xlab("Year")

# Fight time 
menTime <- fights %>% filter(femaleFight==FALSE) %>% 
    group_by(year) %>% summarize(avgTime=mean(fight_duration, na.rm=TRUE))

femaleTime <- fights %>% filter(femaleFight==TRUE) %>% 
  group_by(year) %>% summarize(avgTime=mean(fight_duration, na.rm=TRUE))

fightTime <- merge(menTime, femaleTime, by="year")
names(fightTime)[2:3] <- c("Men Fight Time", "Women Fight Time")
fightTime2 <- melt(fightTime, id="year")
ggplot(data=fightTime2, aes(x=year, y=value, color=variable))+geom_line()+
  ggtitle("MMA Fights Are Getting Longer")+ylab("Average Fight Duraction (in seconds)")+xlab("Year")

# Submission %


ggplot(data=allFighters, aes(x=subWinPct, y=avgWinTime))+geom_point()+geom_smooth(method="lm")





