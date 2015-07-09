# NBA free agency

setwd("~/data-analysis/nba-free-agency/")

require(readr)
require(dplyr)
require(ggplot2)

rawData <- read_csv("data-from-neil.csv")