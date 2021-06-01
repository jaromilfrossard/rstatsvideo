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


auth_as("rstatsvideo")
source("youtube_oauth.R")


tb_channel <- readr::read_delim("data/list_channel.txt",delim=";",
                                col_types = cols(
                                  id_channel = col_character(),
                                  name_channel = col_character()))

#update channels videos
walk(tb_channel$id_channel,update_channel_video)


##tweet_new videos
tweet_videos(tweet_older = T)

