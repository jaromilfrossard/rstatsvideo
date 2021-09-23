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

source("function/create_channels_directory.R")
source("function/update_channel_video.R")
source("function/load_current_video.R")
source("function/video_url.R")
source("function/tibble_video_info.R")
source("function/choose_videos.R")
source("function/write_tweet.R")
source("function/post_videos.R")
source("function/follow_channels.R")
source("function/validate_new_videos.R")
source("function/validate_channel.R")
source("function/check_videos.R")

auth_as("rstatsvideo")
source("youtube_oauth.R")



gert::git_pull()

tb_channel <- readr::read_delim("data/list_channel.txt", delim=";",
                                col_types = cols(
                                  id_channel = col_character(),
                                  name_channel = col_character(),
                                  id_twitter = col_character()),
                                lazy = FALSE)

#update channels videos
walk(tb_channel$id_channel, update_channel_video)

#update_channel_video(id = "UCMdihazndR0f9XBoSXWqnYg")

# tb_channel%>%
#   tail()
# validate_channel("UCfB2mPM4oU5pMgh9gbEqpWA")

validate_new_videos()


videos <- choose_videos()

# 
# #################
# tb_description<- 
#   videos%>%
#   mutate(temp = map(id_video,tibble_video_infos))%>%
#   unnest(temp)%>%
#   select(-id_twitter,id_channel,-ymd_hms_video,-new_video)
# 
# 
# tb_description%>%
#   mutate(description_video = str_trim(description_video))%>%
#   pull(description_video)
# 
# tb_description%>%
#   select(-description_video)%>%
#   print.data.frame()
# 
# 
# ##################


# i = 4
# name_channel = videos$name_channel[i];id_twitter = videos$id_twitter[i]
# id_video = videos$id_video[i]
# new_video= videos$new_video[i]
# write_tweet(name_channel, id_twitter, id_video, new_video)


videos<- 
  videos%>%
  mutate(tweet = pmap_chr(list(name_channel, id_twitter, id_video, new_video),write_tweet))



videos%>%
  {pwalk(list(.$id_channel, .$id_video, .$ymd_hms_video,.$tweet),post_videos)}

##tweet_new videos
#tweet_videos(tweet_older = T)


##follow new channles
follow_channels()


commit_message <- paste0("Update ", format(Sys.Date(), "%Y_%m_%d"))
gert::git_commit_all(commit_message)

gert::git_push()
