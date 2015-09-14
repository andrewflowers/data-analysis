# Rick Perry's Polling Over Time

setwd("~/editing/micah/")

data <- read.csv("./perry_andrew.csv")

source("~/data-analysis/Five38Thm.R")

require(dplyr)
require(ggplot2)
require(lubridate)

data$End.Date <- mdy(data$End.Date)
data$year <- year(data$End.Date)

data <- data %>% filter(year>=2011)

ggplot(data=data, aes(End.Date, Perry))+
  geom_point()+
  geom_smooth(data=subset(data, year<2013), se=F)+
  geom_smooth(data=subset(data, year>=2013), se=F)+
  ylim(c(0, 40))+
  xlab("Date")+ylab("Percent")+
  ggtitle("Rick Perry's Polling Over Time") + Five38Thm