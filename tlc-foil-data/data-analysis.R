# TLC FOIL data analysis
# Andrew Flowers

setwd("~/data-analysis/tlc-foil-data/")

require(readr)
require(dplyr)
require(ggplot2)

uberData <- read_csv("uber-raw-data.csv")
uberData2 <- read_csv("Uber-Jan-Feb-FOIL.csv")

lyftData <- read_csv("Lyft_B02510.csv")
