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


auth_as("rstatsvideo")
source("youtube_oauth.R")

gert::git_pull()

tb_channel <- readr::read_delim("data/list_channel.txt", delim=";",
                                col_types = cols(
                                  id_channel = col_character(),
                                  name_channel = col_character(),
                                  id_twitter = col_character()))

#update channels videos
walk(tb_channel$id_channel, update_channel_video)


videos <- choose_videos()

tb_description<- 
  videos%>%
  mutate(temp = map(id_video,tibble_video_infos))%>%
  unnest(temp)%>%
  select(-id_twitter,id_channel,-ymd_hms_video,-new_video,-lan_video)


tb_description%>%
  pull(description_video)

tb_description%>%
  select(-description_video)%>%
  print.data.frame()


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
