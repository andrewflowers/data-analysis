# YouTube Data Analysis
# Andrew <andrew.flowers@fivethirtyeight.com>
setwd("~/data-analysis/youtube/")

require(readr)
require(dplyr)
require(tidyr)
require(ggplot2)

# Load data
channels <- read_csv("_0_Channels_By_Category.csv")
first_video <- read_csv("_a_First_Video.csv")
vpm <- read_csv("_b_Videos_Per_Month.csv")
wtpm <- read_csv("_c_WT_Per_Month.csv")
spm <- read_csv("_d_Subscribers_By_Month.csv")
all_likes <- read_csv("_e_All_Time_Likes.csv")

# Analysis

# Ideas
# group_by channels, plot videos, views, watched hours and subscribers per month

byMonth <- vpm %>% right_join(channels, by="external_id") %>% 
  group_by(date) %>% summarize(total_videos=sum(videos), total_views=sum(view_count))

ggplot(data=byMonth, aes(x=date))+geom_line(aes(y=total_views), color="green")+ggtitle("Total Views")


