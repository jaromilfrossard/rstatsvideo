tweet_videos <- function(file_tweet = "data/tweets.txt",tweet_older = TRUE){
  if(!file.exists(file_tweet)){
    ## first tweets
    tb_videos <- load_current_video()
    tb_tweet<-
    tb_videos%>%
      arrange(desc(ymd_hms_video))%>%
      slice(1:3)%>%
      mutate(info = map(id_video,tibble_video_infos))%>%
      unnest(info)%>%
      mutate(tweet = pmap_chr(list(id_video,title_video),function(id,title){
        write_tweet_txt(id,title)
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
    
    tb_videos_new <- 
      tb_videos_old %>%
      filter(ymd_hms_video>max(tb_tweet_old$ymd_hms_video))
    
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
        mutate(info = map(id_video,tibble_video_infos))%>%
        unnest(info)%>%
        mutate(tweet = pmap_chr(list(id_video,title_video),function(id,title){
          write_tweet_txt(id,title,header = "#rstats video: ")}))

      
      
    }else if(nrow(tb_videos_new)>0L){
      tb_tweet_new<-
        tb_videos_new%>%
        arrange(desc(ymd_hms_video))%>%
        mutate(info = map(id_video,tibble_video_infos))%>%
        unnest(info)%>%
        mutate(tweet = pmap_chr(list(id_video,title_video),function(id,title){
          write_tweet_txt(id,title)}))
      }
      
    if(!is.null(tb_tweet_new)){
      tb_tweet_new<- 
        tb_tweet_new%>%
        select(id_channel,id_video,ymd_hms_video,tweet)
      
      lapply(tb_tweet_new$tweet, rtweet::post_tweet)
      
      tb_tweet_new <- 
        bind_rows(tb_tweet_old,
                tb_tweet_new)
      
      write_delim(x = tb_tweet_new, file = file_tweet,delim=";")}
      
      
      
    }
    
  }
  
