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
source("function/validate_channel.R")
source("function/validate_new_channels.R")
source("function/validate_new_videos.R")
source("function/write_tweet.R")
source("function/standardise_language.R")

source("function/update_performance_ratio.R")
source("function/update_performance_oldest.R")


auth_as("rstatsvideo")
source("youtube_oauth.R")



gert::git_pull()

tb_channel <- readr::read_delim("data/list_channel.txt", delim=";",
                                col_types = cols(
                                  id_channel = col_character(),
                                  name_channel = col_character(),
                                  id_twitter = col_character()),
                                lazy = FALSE)


# tb_channel%>%
#   arrange(name_channel)%>%
#   readr::write_delim(file="data/list_channel.txt", delim=";",na = "")


validate_new_channels()
#validate_channel("")


#update channels videos


walk(tb_channel$id_channel, update_channel_video)


validate_new_videos()

videos <- choose_videos(max_tweet = 7)
#videos <- videos%>%filter(new_video)

# 
# #################
# videos<-load_current_video()
# 
# videos%>%
#   filter(id_channel=="UClGaKSIiaYp8o2SB3D3aS_A")%>%
#   check_videos()



# 
# tibble_video_infos(id_video)
# i = 4
# name_channel = videos$name_channel[i];id_twitter = videos$id_twitter[i]
# id_video = videos$id_video[i]
# new_video= videos$new_video[i]
# write_tweet(name_channel, id_twitter, id_video, new_video)


videos<- 
  videos%>%
  mutate(tweet = pmap_chr(list(name_channel, id_twitter, id_video, new_video),write_tweet))



videos%>%
  arrange(ymd_hms_video)%>%
  {pwalk(list(.$id_channel, .$id_video, .$ymd_hms_video,.$tweet),post_videos,delay=300)}



###updating performance
tb_perf <- update_performance_oldest(100)
tb_perf <- update_performance_ratio(100)


write_delim(tb_perf, file= "data/performance.csv", delim = ";")





##follow new channels
follow_channels(silent=F)


##update readme/dashboard
rmarkdown::render("README.Rmd", rmarkdown::md_document())
rmarkdown::render("index.Rmd",output_file = "docs/index.html")



commit_message <- paste0("Update ", format(Sys.Date(), "%Y_%m_%d"))
gert::git_commit_all(commit_message)

gert::git_push()

