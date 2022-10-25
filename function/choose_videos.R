choose_videos <- function(file_tweet = "data/tweets.txt",
                          file_channel = "data/list_channel.txt",
                          file_channel_to_rm = "data/list_channel_to_rm.txt",
                          file_video_to_rm = "data/list_video_to_rm.txt",
                          max_tweet = 20,
                          tweet_older = TRUE,
                          n_default = 3){
  
  tb_channel <- read_delim(file_channel,delim=";",
                           col_types = cols(
                             id_channel = col_character(),
                             name_channel = col_character(),
                             id_twitter = col_character()
                           ),
                           lazy = FALSE)
  
  ## remove channels
  if(file.exists(file_channel_to_rm)){
    tb_channel_to_rm <- read_delim(file_channel_to_rm,delim=";",
                                   col_types = cols(
                                     id_channel = col_character(),
                                     name_channel = col_character()
                                   ),
                                   lazy = FALSE)
    
    tb_channel <-
      anti_join(tb_channel, tb_channel_to_rm, by = c("id_channel"="id_channel",
                                                     "name_channel" = "name_channel"))
  }
  
  tb_videos <- load_current_video()
  
  
  if(file.exists(file_video_to_rm)){
    tb_video_to_rm <- read_delim(file_video_to_rm,delim=";",
                                 col_types = cols(
                                   id_channel = col_character(),
                                   id_video = col_character()
                                 ),
                                 lazy = FALSE)
    tb_videos<- 
      tb_videos%>%
      anti_join(tb_video_to_rm,by =c("id_channel" = "id_channel","id_video"="id_video"))
  }
  
  
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
    filter(ymd_hms_video>(max(tb_tweet_old$ymd_hms_video)-60*60*24))%>%
    anti_join(select(tb_tweet_old,id_video),by = c("id_video"="id_video"))%>%
    filter(id_channel%in%tb_channel$id_channel)
  
  
  if(nrow(tb_videos_new)>0&nrow(tb_videos_new)<=max_tweet){
    tb_videos_selected <- 
      tb_videos_new%>%
      arrange(ymd_hms_video)%>%
      mutate(new_video = TRUE)
    
    if(nrow(tb_videos_selected)<n_default){
      tb_videos_selected<-
        bind_rows(tb_videos_selected,(
          tb_videos%>%
            filter(!id_video%in%tb_tweet_old$id_video)%>%
            slice(sample(1:n(),n_default-nrow(tb_videos_selected)))%>%
            mutate(new_video = FALSE)))
      
    }
    
    }else if(nrow(tb_videos_new)>max_tweet){
      message(glue("The tweets limits ({max_tweet}) is less that the number of new videos ({nrow(tb_videos_new)})."))
      tb_videos_selected <- 
        tb_videos_new%>%
        arrange(ymd_hms_video)%>%
        slice(1:max_tweet)%>%
        mutate(new_video = TRUE)
    }else if(nrow(tb_videos_new)==0){
      tb_videos_selected <-
        tb_videos%>%
        filter(!id_video%in%tb_tweet_old$id_video)%>%
        slice(sample(1:n(),n_default))%>%
        mutate(new_video = FALSE)

  }
  
  tb_videos_selected%>%
    left_join(tb_channel,  by = c("id_channel" = "id_channel"))%>%
    select(name_channel,id_twitter,id_channel,id_video,ymd_hms_video,new_video)
                
  
}