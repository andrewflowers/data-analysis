# Andrew Flowers
# andrew.flowers@fivethirtyeight.com

require(readr)
require(RODBC)
require(dplyr)
require(ggplot2)
require(stringr)

setwd("~/data-analysis/ufc/")

fights <- read_csv("raw_ufc_data.csv")

ronda <- "Ronda Rousey"

# Female records
femRecs <- fights %>% filter(fighter1_gender=="F" & fighter2_gender=="F") %>% 
  group_by(fight_winner) %>% summarize(n=n()) %>% arrange(desc(n))

# Rousey fights
rouseyFights <- fights %>% filter(fighter1==ronda | fighter2==ronda)

# Ideas to explore
# Breakdown fights by ending, plot over time
# Isolate armbar submission, see how that's changed since Rousey

# TODO
# Clean submission data



