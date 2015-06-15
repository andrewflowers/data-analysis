require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)
require(reshape2)
require(zoo)
require(scales)


# Arrests
arrests <- read_csv("BPD_Arrests.csv")
arrests$ArrestDate <- as.Date(arrests$ArrestDate, "%m/%d/%Y")

start <- as.Date('1/6/2013',"%m/%d/%Y")
end <- as.Date('6/6/2015',"%m/%d/%Y")

arrests<-subset(arrests,as.Date(arrests$ArrestDate, "%m/%d/%Y") >= start)
arrests<-subset(arrests,as.Date(arrests$ArrestDate, "%m/%d/%Y")<= end)

arrests$Week <- as.Date(cut(arrests$ArrestDate,breaks="week",start.on.monday=FALSE))
arrests_byWeek <- arrests %>% group_by(Week) %>% summarize(total_arrests=n())

arrest_plot <- ggplot(data=arrests_byWeek, aes(x=Week, y=total_arrests))+
  geom_line()+
  #geom_smooth()+
  scale_y_continuous(limits=c(0,1000))+
  scale_x_date(breaks="years",labels=date_format("%Y"))+
  Five38Thm
  #ggtitle("Weekly Arrests by Baltimore Police Department, from 1/6/2013 through 6/6/2015")

print(arrest_plot)

#Crime

crime <- read_csv("BPD_Part_1_Victim_Based_Crime_Data.csv")
crime$CrimeDate <- as.Date(crime$CrimeDate, "%m/%d/%y")

start <- as.Date('1/2/2011',"%m/%d/%Y")
end <- as.Date('6/6/2015',"%m/%d/%Y")

crime<-subset(crime,as.Date(crime$CrimeDate, "%m/%d/%Y") >= start)
crime<-subset(crime,as.Date(crime$CrimeDate, "%m/%d/%Y") <= end)

crime$Week <- as.Date(cut(crime$CrimeDate,breaks="week",start.on.monday=FALSE))
crime_byWeek <- crime %>% group_by(Week) %>% summarize(total_crimes=n())

crime_plot<-ggplot(data=crime_byWeek, aes(x=Week, y=total_crimes))+
  geom_line()+
  #geom_smooth()+
  Five38Thm+
  scale_y_continuous(limits=c(0,1200))
  #ggtitle("Total Crimes by week (source: Baltimore Police Department) from 1/3/2010 through 6/6/2015")

#print(crime_plot)


#Homicides
#crimeType <- dcast(crime, formula=CrimeDate~Description)
homicide<-select(crimeType,CrimeDate,HOMICIDE,SHOOTING)

start <- as.Date('1/2/2011',"%m/%d/%Y")
end <- as.Date('6/6/2015',"%m/%d/%Y")

homicide<-subset(homicide,as.Date(homicide$CrimeDate, "%m/%d/%Y") >= start)
homicide<-subset(homicide,as.Date(homicide$CrimeDate, "%m/%d/%Y") <= end)

homicide$Week <- as.Date(cut(homicide$CrimeDate,breaks="week",start.on.monday=FALSE))
print(head(homicide))
homicide_byWeek <- homicide %>% group_by(Week) %>% summarize(total_crimes=sum(HOMICIDE,SHOOTING))

homicide_plot<-ggplot(data=homicide_byWeek, aes(x=Week, y=total_crimes))+
  geom_line()+
  #geom_smooth()+
  Five38Thm+
  scale_y_continuous(limits=c(0,40))
#ggtitle("Total Crimes by week (source: Baltimore Police Department) from 1/3/2010 through 6/6/2015")

print(homicide_plot)

#Violent
start <- as.Date('1/2/2011',"%m/%d/%Y")
end <- as.Date('6/6/2015',"%m/%d/%Y")

violent <- crimeType %>% 
  group_by(CrimeDate) %>%
  summarize(total = sum(`AGG. ASSAULT`,`ASSAULT BY THREAT`,`COMMON ASSAULT`, HOMICIDE,RAPE,`ROBBERY - CARJACKING`,`ROBBERY - COMMERCIAL`,`ROBBERY - RESIDENCE`,`ROBBERY - STREET`,SHOOTING))

violent<-subset(violent,as.Date(violent$CrimeDate, "%m/%d/%Y") >= start)
violent<-subset(violent,as.Date(violent$CrimeDate, "%m/%d/%Y") <= end)

violent$Week <- as.Date(cut(violent$CrimeDate,breaks="week",start.on.monday=FALSE))
violent_byWeek <- violent %>% group_by(Week) %>% summarize(violent_crimes=sum(total))

violent_plot<-ggplot(data=violent_byWeek, aes(x=Week, y=violent_crimes))+
  geom_line()+
  #geom_smooth()+
  Five38Thm+
  scale_y_continuous(limits=c(0,500))

print(violent_plot)
