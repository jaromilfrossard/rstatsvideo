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


videos<-load_current_video()
tb_channel_to_rm <- read_delim("data/list_channel_to_rm.txt",delim=";",
                               col_types = cols(
                                 id_channel = col_character(),
                                 name_channel = col_character()
                               ),
                               lazy = FALSE)

tb_video_to_rm <- read_delim("data/list_video_to_rm.txt",delim=";",
                               col_types = cols(
                                 id_channel = col_character(),
                                 id_video = col_character()
                               ),
                               lazy = FALSE)


videos <-
  videos%>%
  anti_join(tb_channel_to_rm, by = c("id_channel"="id_channel"))%>%
  anti_join(tb_video_to_rm,by =c("id_channel" = "id_channel","id_video"="id_video"))

videos<-
  videos%>%
  filter(ymd_hms_video>as_date("2018-04-30"))




videos
i = 1
checking = "continue"
while(checking=="continue"){
  
  urli <- video_url(videos$id_video[i])
  browseURL(urli)
  res <- svDialogs::dlg_list(c("accept","reject","stop"))
  
  if(res$res=="reject"){
    tb_video_to_rm <- read_delim("data/list_video_to_rm.txt",delim=";",
                               col_types = cols(
                                 id_channel = col_character(),
                                 id_video = col_character()
                               ),
                               lazy = FALSE)
  
  tb_video_to_rm%>%
    bind_rows(
      tribble(~id_channel,~id_video,
              videos$id_channel[i],videos$id_video[i]))%>%
  write_delim(x =., file = "data/list_video_to_rm.txt",delim=";")
  }
  if(res$res=="stop"){
    
    print(as_date(videos$ymd_hms_video[i])-1)
    checking="stop"}
  i = i+1
  
  }


