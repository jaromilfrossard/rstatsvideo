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
source("function/check_videos.R")



videos<-load_current_video()
tb_channel <- read_delim("data/list_channel.txt",delim=";",
                               col_types = cols(
                                 id_channel = col_character(),
                                 name_channel = col_character()
                               ),
                               lazy = FALSE)
videos<-
  videos%>%
  left_join(tb_channel,by = c("id_channel"= "id_channel"))%>%
  select(name_channel,id_channel,id_video,ymd_hms_video)


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

# videos<-
#   videos%>%
#   filter(!str_detect(tolower(name_channel),"ladies"))%>%
#   filter(!name_channel%in%c("RStudio","R Consortium","NHSR Community",
#                             "Statistics of DOOM","satRdays","Lander Analytics",
#                             "Shiny DeveloperSeries","Statistics Globe",
#                             "Why R? Foundation","R4DS Online Learning Community",
#                             "Julia Silge"))%>%
#   filter(ymd_hms_video>as_date("2021-09-19"))



videos%>%
  filter(name_channel=="dataAnalysisR")%>%
  check_videos()





