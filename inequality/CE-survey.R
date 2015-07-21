# Andrew Flowers
# andrew.flowers@fivethiryteight.com

# Consumer Expenditure survey data analysis

require(readr)
require(dplyr)
require(ggplot2)

setwd("~/data-analysis/inequality")

# Load expn files
load("./microdata/2013/expn/fra13.rda") # Home Furnishings and Related Household Items – Purchases
load("./microdata/2013/expn/apa13.rda") # Appliances, Household Equipment, and Other Selected Items – Purchase of Household Appliances

# Load fmli files for latest calendar year (2013)
load("./microdata/2013/intrvw/fmli131x.rda")
load("./microdata/2013/intrvw/fmli132.rda")
load("./microdata/2013/intrvw/fmli133.rda")
load("./microdata/2013/intrvw/fmli134.rda")
load("./microdata/2013/intrvw/fmli141.rda")

# Extract data on all 2013 CU's -- newid, inclass2, etc.
vars <- c("newid", "inclass2", "finlwt21")
consumers <- rbind(fmli131[,vars], fmli131x[,vars], fmli132[,vars], fmli133[,vars], fmli134[,vars], fmli141[,vars])

# APA (appliances) expn file
items <- c(100, 120, 140, 160, 190, 200)
itemDesc <- c("stove", "microwave", "refrigerator", "dishwasher", "washer", "dryer")
# NOTE: need to deal with combined appliance purchases, where MAJAPPY = 220 or 225

appPurchases <- apa13 %>% 
  mutate(inclass2=consumers[match(apa13$newid, consumers$newid),]$inclass2) %>% 
  mutate(incTercile=ifelse(inclass2<=2, "bottom", ifelse(inclass2<=4 & inclass2>2, "middle", ifelse(inclass2<7 & inclass2>4, "top", "none")))) %>% 
  filter(majapply %in% items, gftc_maj==1)






# Join income category to expn files
fra13$inclass <- fmli131x[match(fra13$newid, fmli131x$newid),]$inclass
fra13$inclass <- fmli131x[match(fra13$newid, fmli131x$newid),]$inclass
sofas <- fra13 %>% filter(furnpury==100, furngftc==1) %>% 
  group_by(inclass) %>% 
  summarize(n=n(),
            median=median(as.numeric(furnpurx)))

# Alternative income classification
fra13$inclass2 <- fmli131x[match(fra13$newid, fmli131x$newid),]$inclass

# Use inclass2, but just 
# example: table(fmli131x$inclass2)

# Weights: use final weight, finlwt21


# Kitchen/living room items to classify:
# FRA
# sofa (100)
# APA
# 


items <- c(100)








