# Andrew Flowers
# andrew.flowers@fivethiryteight.com

# Consumer Expenditure survey data analysis

require(readr)
require(dplyr)
require(ggplot2)

setwd("~/data-analysis/inequality")

# Example data analysis
# Sofas, specifically

# Load expn file
load("./microdata/2013/expn/fra13.rda")
load("./microdata/2013/intrvw/fmli131x.rda")

# Join income category to expn file
fra13$inclass <- fmli131x[match(fra13$newid, fmli131x$newid),]$inclass

sofas <- fra13 %>% filter(furnpury==100, furngftc==1) %>% 
  group_by(inclass) %>% 
  summarize(median=median(as.numeric(furnpurx)))

