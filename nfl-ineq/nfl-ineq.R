# NFL inequality analysis

require(readr)
require(dplyr)
require(ineq)

rawData <- read_csv("raw-nfl-data.csv")

# Gini coefficients for Elo and wins, sorted by those for Elo
gini <- rawData %>% group_by(year_id) %>% 
  summarize(gini_Elo=Gini(elo_talent_wins),
            gini_wins=Gini(norm_W)) %>% 
  arrange(desc(gini_Elo))

# Just for wins
giniWins <- rawData %>% group_by(year_id) %>% 
  summarize(gini_wins=Gini(norm_W)) %>% 
  arrange(desc(gini_wins))

giniWins

teams_1986 <- rawData %>% filter(year_id==1986)

undefeatedTeams <- rawData %>% 
  mutate(undefeated=ifelse(WPCT==1, 1, 0)) %>% 
  group_by(year_id) %>% 
  summarize(n_undefeated=sum(undefeated)) %>% 
  arrange(desc(n_undefeated))
    
# Typical win breakdown over 36 seasons
table(rawData$wins)/1081
table(rawData$norm_W)/1081

rawData %>% group_by(year_id) %>% 
  summarize(wins_0=tally(norm_W==1.0))

normWins <- rawData %>% count(norm_W) %>% mutate(share=n/nrow(rawData))

years <- unique(rawData$year_id)

for (y in years){
  yearData <- rawData %>% filter(year_id==y)
  teams <- mean(yearData$no_tms)
  newData <- yearData %>% count(norm_W) %>% mutate(share=n/teams)
  normWins[[as.character(y)]] <- newData[match(normWins$norm_W, newData$norm_W),]$share
}

write_csv(normWins, "norm-wins-deviation.csv")
