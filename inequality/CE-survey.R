# Andrew Flowers
# andrew.flowers@fivethiryteight.com

# Consumer Expenditure survey data analysis

require(readr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(cwhmisc)
require(xlsx)
require(survey)

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

# Load UCC codes for 2013 data
uccCodes <- read_csv("ucc_codes_2013.csv")

# Create survey attribute
# TK

# Extract data on all 2013 CU's -- newid, inclass2, etc.
vars <- c("newid", "inclass2", "finlwt21")
consumers <- rbind(fmli131x[,vars], fmli132[,vars], fmli133[,vars], fmli134[,vars], fmli141[,vars])

consumersByInc <- consumers %>%
  mutate(incTercile=ifelse(inclass2<=2, "bottom", ifelse(inclass2<=4 & inclass2>2, "middle", ifelse(inclass2<7 & inclass2>4, "top", "none")))) %>% 
  group_by(incTercile) %>% 
  summarize(n=sum(finlwt21)) %>% 
  filter(incTercile!="none")

# APA (appliances) expn file
items <- c(100, 120, 140, 160, 190, 200)
itemDesc <- as.data.frame(c("stove", "microwave", "refrigerator", "dishwasher", "washer", "dryer"))
colnames(itemDesc) <- "item"
# NOTE: need to deal with combined appliance purchases, where MAJAPPY = 220 or 225

appPurchases <- apa13 %>% 
  mutate(weight=consumers[match(apa13$newid, consumers$newid),]$finlwt21) %>%
  mutate(inclass2=consumers[match(apa13$newid, consumers$newid),]$inclass2) %>% 
  mutate(incTercile=ifelse(inclass2<=2, "bottom", ifelse(inclass2<=4 & inclass2>2, "middle", ifelse(inclass2<7 & inclass2>4, "top", "none")))) %>% 
  filter(majapply %in% items, gftc_maj==1, incTercile!="none") %>% 
  select(newid, majapply, weight, majpurx, incTercile) 

appPrice <- appPurchases %>% 
  dcast(formula=majapply~incTercile, value.var="majpurx", fun.aggregate=median, fill=NaN) %>%
  # NOTE: this median calculation is UNWEIGHTED
  cbind(itemDesc) %>% select(item, 2:4)

appFreq <- appPurchases %>% 
  dcast(formula=majapply~incTercile,  value.var="weight", fun.aggregate=sum, fill=NaN) %>%
  cbind(itemDesc) %>% select(item, 2:4)
appFreq[,2:4] <- (appFreq[,2:4]/t(consumersByInc$n))*100

# FRA (furniture) expn file
items <- c(100, 101, 102, 103, 120, 141, 171, 190, 191, 192, 193, 214, 215)
itemDesc <- as.data.frame(c("sofas", "living room chairs", "living room tables", 
                            "shelves/cabinets", "mattresses/springs", "bbq grill", 
                            "lamps", "plastic dinnerware", "china dinnerware", "stainless/silver flatware",
                            "glassware", "curtains/drapes", "blinds/shades"
                            ))
colnames(itemDesc) <- "item"
# NOTE: need to deal with combined furniture purchases, where FURNNPURY = 106, 122, 160, 197, 204, 220, 900

furPurchases <- fra13 %>% 
  mutate(weight=consumers[match(fra13$newid, consumers$newid),]$finlwt21) %>%
  mutate(inclass2=consumers[match(fra13$newid, consumers$newid),]$inclass2) %>% 
  mutate(incTercile=ifelse(inclass2<=2, "bottom", ifelse(inclass2<=4 & inclass2>2, "middle", ifelse(inclass2<7 & inclass2>4, "top", "none")))) %>% 
  filter(furnpury %in% items, furngftc==1, incTercile!="none") %>% 
  select(newid, furnpury, weight, furnpurx, incTercile) 

furPrice <- furPurchases %>% 
  dcast(formula=furnpury~incTercile, value.var="furnpurx", fun.aggregate=median, fill=NaN) %>%
  # NOTE: this median calculation is UNWEIGHTED
  cbind(itemDesc) %>% select(item, 2:4)

furFreq <- furPurchases %>% 
  dcast(formula=furnpury~incTercile,  value.var="weight", fun.aggregate=sum, fill=NaN) %>%
  cbind(itemDesc) %>% select(item, 2:4)
furFreq[,2:4] <- (furFreq[,2:4]/t(consumersByInc$n))*100

itemPrices <- rbind(appPrice, furPrice)
itemFreq <- rbind(appFreq, furFreq)

wb <- createWorkbook()
median_price <- createSheet(wb, sheetName="median_price")
addDataFrame(itemPrices, median_price)
pct_purchasing <- createSheet(wb, sheetName="pct_purchasing")
addDataFrame(itemFreq, pct_purchasing)
saveWorkbook(wb, "new_kitchen_and_living_room.xlsx")
