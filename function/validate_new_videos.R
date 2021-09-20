validate_new_videos <- function(file_tweet = "data/tweets.txt",
                                file_channel = "data/list_channel.txt",
                                file_channel_to_rm = "data/list_channel_to_rm.txt",
                                file_video_to_rm = "data/list_video_to_rm.txt"){
  
  tb_channel <- read_delim(file_channel,delim=";",
                           col_types = cols(
                             id_channel = col_character(),
                             name_channel = col_character(),
                             id_twitter = col_character()
                           ),
                           lazy = FALSE)
  
  
  tb_videos <- load_current_video()
  
  tb_channel_to_rm <- read_delim(file_channel_to_rm,delim=";",
                                 col_types = cols(
                                   id_channel = col_character(),
                                   name_channel = col_character()
                                 ),
                                 lazy = FALSE)
  
  tb_video_to_rm <- read_delim(file_video_to_rm,delim=";",
                               col_types = cols(
                                 id_channel = col_character(),
                                 id_video = col_character()
                               ),
                               lazy = FALSE)
  
  
  tb_videos<-
    tb_videos%>%
    anti_join(tb_channel_to_rm, by = c("id_channel"="id_channel"))%>%
    anti_join(tb_video_to_rm,by =c("id_video"="id_video"))
  
  tb_tweet_old <-read_delim(file_tweet,delim=";",
                            col_types = cols(
                              id_channel = col_character(),
                              id_video = col_character(),
                              ymd_hms_video = col_datetime(),
                              tweet = col_character()
                            ),
                            lazy = FALSE)%>%
    mutate(ymd_hms_video = ymd_hms(ymd_hms_video))
  
  
  tb_videos_new <- 
    tb_videos %>%
    filter(ymd_hms_video>max(tb_tweet_old$ymd_hms_video))%>%
    filter(id_channel%in%tb_channel$id_channel)
  
  check_videos(tb_videos_new)
  
  
  
  
  
  
  
  
}