# Super PACs Distribution

setwd("~/data-analysis/super-PACs/")
source("~/data-analysis/Five38Thm.R")

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
            n_top5=sum(ifelse(top5, 1, 0)),
            from_top5=sum(ifelse(top5, total, 0)), 
            n_bottom5=sum(ifelse(bottom5, 1, 0)),
            from_bottom5=sum(ifelse(bottom5, total, 0)), 
            ratio=from_top5/from_bottom5,
            top5_share=(from_top5/total_raised)*100) %>% 
  arrange(desc(ratio))

ggplot(data=dist, aes(x=avg_donor, y=ratio, label=supports))+
  geom_point()+geom_text()+xlab("Average SuperPAC Donation")+ylab("Ratio of top 5% to bottom 5%")

ggplot(data=dist, aes(x=n_top5, y=top5_share, label=supports))+
  geom_point()+geom_text(vjust=-1)+xlab("Number of donors in top 5%")+ylab("Percentage from top 5% of donors") + Five38Thm
