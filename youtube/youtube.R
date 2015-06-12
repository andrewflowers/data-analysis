# YouTube Data Analysis
# Andrew <andrew.flowers@fivethirtyeight.com>
setwd("~/data-analysis/youtube/")

require(readr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(reshape2)

# Load data
channels <- read_csv("_0_Channels_By_Category.csv")
first_video <- read_csv("_a_First_Video.csv")
vpm <- read_csv("_b_Videos_Per_Month.csv")
wtpm <- read_csv("_c_WT_Per_Month.csv")
spm <- read_csv("_d_Subscribers_By_Month.csv")
all_likes <- read_csv("_e_All_Time_Likes.csv")

# Analysis

# Total stats by channels
total_sub <- spm %>% mutate(channel=channels[match(external_id, channels$external_id),]$public_name) %>% 
  group_by(channel) %>% 
  summarize(total_sub=sum(subscriber_count))

total_vid <- vpm %>% mutate(channel=channels[match(external_id, channels$external_id),]$public_name) %>% 
  group_by(channel) %>% 
  summarize(total_videos=sum(videos),
            total_views=sum(view_count))

total_hours <- wtpm %>% mutate(channel=channels[match(external_id, channels$external_id),]$public_name) %>% 
  group_by(channel) %>% 
  summarize(total_hours=sum(watch_hours))

totals <- all_likes %>% mutate(channel=channels[match(external_id, channels$external_id),]$public_name) %>%
  right_join(total_sub, by="channel") %>% 
  right_join(total_vid, by="channel") %>% 
  right_join(total_hours, by="channel") %>% 
  select(channel, likes, total_sub, total_hours, total_videos, total_views) %>% 
  arrange(desc(total_views)) %>% 
  mutate(sub_per_hour=total_sub/total_hours,
         sub_per_view=total_sub/total_views,
         views_per_vid=total_views/total_videos,
         hours_per_vid=total_hours/total_videos,gi
         likes_per_hour=likes/total_hours)

# Create tidy by month data set
# group_by channels, plot videos, views, watched hours and subscribers per month

byMonth_videos <- vpm %>% 
  mutate(channel=channels[match(external_id, channels$external_id),]$public_name,
         category=channels[match(external_id, channels$external_id),]$category) %>% 
  select(date, channel, category, videos) %>% 
  distinct(date, channel, category) %>% 
  filter(!is.na(channel)) %>%
  mutate(name=paste0(channel, "_", category)) %>% 
  dcast(formula=date~name, value.var="videos")

ggplot(data=byMonth, aes(x=date))+geom_line(aes(y=total_views), color="green")+ggtitle("Total Views")



