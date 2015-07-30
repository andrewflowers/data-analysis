# Neil Paine
#setwd()

reg <- glm(made_playoffs~DOFF+at_time_pyth+preseason_talent,family=binomial(link="logit"))

summary(reg)

plot(reg$fitted ~ factor(made_playoffs))