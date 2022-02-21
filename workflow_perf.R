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
library(gert)
library(rvest)

#library(textcat)

source("function/channel_exist.R")
source("function/channel_url.R")

source("function/check_videos.R")
source("function/choose_videos.R")
source("function/create_channels_directory.R")
source("function/follow_channels.R")
source("function/load_current_video.R")
source("function/tibble_video_info.R")
source("function/tibble_video_stats.R")
source("function/update_channel_video.R")
source("function/video_url.R")
source("function/post_videos.R")
source("function/update_performance2.R")
source("function/validate_channel.R")
source("function/validate_new_videos.R")
source("function/write_tweet.R")


#auth_as("rstatsvideo")
source("youtube_oauth.R")


tb_perf <- update_performance2(50)

write_delim(tb_perf, file= "data/performance.csv", delim = ";")
#tb_perf%>%filter(id_video=="ytCTzUoQg90")%>%transmute(del = paste(id_channel,id_video,sep = ";"))

rmarkdown::render("README.Rmd", rmarkdown::md_document())



rmarkdown::render("index.Rmd",output_file = "docs/index.html")



