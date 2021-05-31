rm(list=ls())
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tuber)
library(readr)
library(lubridate)
library(glue)
library(rtweet)

source("function/create_channels_directory.R")
source("function/update_channel_video.R")
source("function/load_current_video.R")
source("function/video_url.R")
source("function/tibble_video_info.R")
source("function/tweet_videos.R")
source("function/write_tweet_txt.R")

yt_oauth("254514166834-4gai69ivsgba5cl7il7valh1lkvtv8vl.apps.googleusercontent.com", 
         "Cb7YSYe8rCcoDxLIcYnwxvC8", token = "")
auth_as("rstatsvideo")



tb_channel <- readr::read_delim("data/list_channel.txt",delim=";",
                                col_types = cols(
                                  id_channel = col_character(),
                                  name_channel = col_character()))

#update channels videos
walk(tb_channel$id_channel,update_channel_video)


##tweet_new videos
tweet_videos(tweet_older = F)



tb_videos <- load_current_video()
  
tb_videos%>%
  filter(ymd_hms_video== max(ymd_hms_video))


