# NFL inequality analysis

require(readr)
require(dplyr)
require(ineq)

rawData <- read_csv("raw-nfl-data.csv")

# Gini coefficients for Elo and wins, sorted by those for Elo
rawData %>% group_by(year_id) %>% 
  summarize(gini_Elo=Gini(elo_talent_wins),
            gini_wins=Gini(norm_W)) %>% 
  arrange(desc(gini_Elo))

# Just for wins
giniWins <- rawData %>% group_by(year_id) %>% 
  summarize(gini_wins=Gini(norm_W)) %>% 
  arrange(desc(gini_wins))

giniWins