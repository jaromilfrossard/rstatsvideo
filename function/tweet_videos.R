tweet_videos <- function(file_tweet = "data/tweets.txt",
                         file_channel = "data/list_channel.txt",
                         file_channel_to_rm = "data/list_channel_to_rm.txt",
                         file_video_to_rm = "data/list_video_to_rm.txt",
                         tweet_older = TRUE){
  
  tb_channel <- read_delim(file_channel,delim=";",
                           col_types = cols(
                             id_channel = col_character(),
                             name_channel = col_character(),
                             id_twitter = col_character()
                           ))
  
  if(file.exists(file_channel_to_rm)){
    tb_channel_to_rm <- read_delim(file_channel_to_rm,delim=";",
                           col_types = cols(
                             id_channel = col_character(),
                             name_channel = col_character()
                           ))
    
    tb_channel <-
      anti_join(tb_channel, tb_channel_to_rm, by = c("id_channel"="id_channel",
                                                   "name_channel" = "name_channel"))
  }
  
  

  


  
  if(!file.exists(file_tweet)){
    ## first tweets
    tb_videos <- load_current_video()
    if(file.exists(file_video_to_rm)){
      tb_video_to_rm <- read_delim(file_video_to_rm,delim=";",
                                   col_types = cols(
                                     id_channel = col_character(),
                                     id_video = col_character()
                                   ))
      tb_videos<- 
        tb_videos%>%
        anti_join(tb_video_to_rm,by =c("id_channel" = "id_channel","id_video"="id_video"))
    }

    
    tb_tweet<-
      tb_videos%>%
      filter(id_channel%in%tb_channel$id_channel)%>%
      arrange(desc(ymd_hms_video))%>%
      slice(1:3)%>%
      left_join(tb_channel,by = "id_channel")%>%
      mutate(info = map(id_video,tibble_video_infos))%>%
      unnest(info)%>%
      mutate(by = case_when(is.na(id_twitter)~name_channel,
                            !is.na(id_twitter)~as.character(glue("{name_channel} (@{id_twitter})"))))%>%
      mutate(tweet = pmap_chr(list(id_video,title_video,by),function(id,title,ni){
        write_tweet_txt(id,title,name_channel = ni)
      }))
    
    tb_tweet<- 
      tb_tweet%>%
      select(id_channel,id_video,ymd_hms_video,tweet)
    
    write_delim(x =tb_tweet, file = file_tweet,delim=";")
    out = lapply(tb_tweet$tweet, rtweet::post_tweet)
    
  }else if(file.exists(file_tweet)){
    ## next tweets
    tb_tweet_old <-read_delim(file_tweet,delim=";",
                               col_types = cols(
                                 id_channel = col_character(),
                                 id_video = col_character(),
                                 ymd_hms_video = col_datetime(),
                                 tweet = col_character()
                               ))%>%
      mutate(ymd_hms_video = ymd_hms(ymd_hms_video))
    
    
    tb_videos_old <- load_current_video()
    if(file.exists(file_video_to_rm)){
      tb_video_to_rm <- read_delim(file_video_to_rm,delim=";",
                                   col_types = cols(
                                     id_channel = col_character(),
                                     id_video = col_character()
                                   ))
      tb_videos<- 
        tb_videos%>%
        anti_join(tb_video_to_rm,by =c("id_channel" = "id_channel","id_video"="id_video"))
    }
    
    tb_videos_new <- 
      tb_videos_old %>%
      filter(ymd_hms_video>max(tb_tweet_old$ymd_hms_video))%>%
      filter(id_channel%in%tb_channel$id_channel)
      
    
    tb_tweet_new = NULL
    
    if(nrow(tb_videos_new)==0L&tweet_older){
      
      tb_tweet_count<-
        tb_tweet_old%>%
        group_by(id_channel,id_video)%>%
        summarise(count = n(),.groups="drop")

      tb_tweet_new<- 
        tb_videos_old%>%
        left_join(tb_tweet_count,by = c("id_channel", "id_video"))%>%
        mutate(count = replace_na(count,0L))
      
      if(sum(tb_tweet_new$count==0)>0L){
        tb_tweet_new<- 
          tb_tweet_new%>%
          filter(count==0L)%>%
          slice(sample(1:n(),3))
          
      }else{
        tb_tweet_new<- 
          tb_tweet_new%>%
          slice(sample(1:n(),3))
      }
      
      tb_tweet_new<-
        tb_tweet_new%>%
        dplyr::select(-count)%>%
        arrange(desc(ymd_hms_video))%>%
        left_join(tb_channel,by = "id_channel")%>%
        mutate(info = map(id_video,tibble_video_infos))%>%
        unnest(info)%>%
        mutate(by = case_when(is.na(id_twitter)~name_channel,
                              !is.na(id_twitter)~as.character(glue("{name_channel} (@{id_twitter})"))))%>%
        mutate(tweet = pmap_chr(list(id_video,title_video,by),function(id,title,ni){
          write_tweet_txt(id,title,header = "#rstats video: ", name_channel = ni)}))

      
      
    }else if(nrow(tb_videos_new)>0L){
      tb_tweet_new<-
        tb_videos_new%>%
        arrange(desc(ymd_hms_video))%>%
        left_join(tb_channel,by = "id_channel")%>%
        mutate(info = map(id_video,tibble_video_infos))%>%
        unnest(info)%>%
        mutate(by = case_when(is.na(id_twitter)~name_channel,
                              !is.na(id_twitter)~as.character(glue("{name_channel} (@{id_twitter})"))))%>%
        mutate(tweet = pmap_chr(list(id_video,title_video,by),function(id,title,ni){
          write_tweet_txt(id,title, name_channel = ni)}))
      }
      
    if(!is.null(tb_tweet_new)){
      tb_tweet_new<- 
        tb_tweet_new%>%
        select(id_channel,id_video,ymd_hms_video,tweet)
      
      lapply(tb_tweet_new$tweet, function(tweeti){
        rtweet::post_tweet(tweeti)
        Sys.sleep(15)
        })
      
      tb_tweet_new <- 
        bind_rows(tb_tweet_old,
                tb_tweet_new)
      
      write_delim(x = tb_tweet_new, file = file_tweet,delim=";")}
      
      
      
    }
    
  }
  
