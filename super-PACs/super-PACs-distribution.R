# Super PACs Distribution

setwd("~/data-analysis/super-PACs/")

require(readr)
require(dplyr)
require(ggplot2)

bycoffe2015 <- read_csv("bycoffe2015.csv")

dist <- bycoffe2015 %>% group_by(supports) %>% 
  mutate(threshold1=as.numeric(quantile(total, probs=c(0.95))),
         top5=ifelse(total>=threshold1, TRUE, FALSE), 
         threshold2=as.numeric(quantile(total, probs=c(0.05))),
         bottom5=ifelse(total<=threshold2, TRUE, FALSE)) %>% 
  summarize(total_raised=sum(total),
            avg_donor=total_raised/n(),
            from_top5=sum(ifelse(top5, total, 0)), 
            from_bottom5=sum(ifelse(bottom5, total, 0)), 
            ratio=from_top5/from_bottom5) %>% 
  arrange(desc(ratio))


