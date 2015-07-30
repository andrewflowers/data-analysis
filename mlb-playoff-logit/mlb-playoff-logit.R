# Neil Paine
#setwd()

data  <- read.csv("rawdata.tsv", sep="\t")

reg <- glm(data=data, made_playoffs~DOFF+at_time_pyth+preseason_talent,family=binomial(link="logit"))

summary(reg)

plot(data$fitted ~ factor(data$made_playoffs))
