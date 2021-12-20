# file_channel = "data/list_channel.txt";
# file_perf = "data/performance.csv";
# file_channel_to_rm = "data/list_channel_to_rm.txt";
# file_video_to_rm = "data/list_video_to_rm.txt";
# credit = 10;
# 


update_performance <- function(credit = 200,
                               file_channel = "data/list_channel.txt",
                               file_perf = "data/performance.csv",
                               file_channel_to_rm = "data/list_channel_to_rm.txt",
                               file_video_to_rm = "data/list_video_to_rm.txt"
                               ){
  
  

  
  
  videos <- load_current_video()
  tb_channel <- read_delim(file_channel,delim=";",
                           col_types = cols(
                             id_channel = col_character(),
                             name_channel = col_character()
                           ),
                           lazy = FALSE)
  
  tb_perf <- readr::read_delim(file_perf, delim =";",col_types = cols(
    name_channel = col_character(), id_channel = col_character(),
    id_video = col_character(), ymd_hms_video = col_datetime(format = ""),
    id_twitter = col_character(), count_view = col_integer(),
    count_comment = col_integer(), url_channel = col_character(),
    url_video = col_character(), ymd_update = col_date(format = "")
  ),lazy = F)
  
  ### update name channel and twitter account
  tb_perf<-
    tb_perf%>%
    select(-name_channel,-id_twitter)%>%
    left_join(tb_channel,by = c("id_channel"= "id_channel"))%>%
    relocate(name_channel,.before=id_channel)%>%
    relocate(id_twitter,.after=ymd_hms_video)
  
  
  
  
  videos<-
    videos%>%
    left_join(tb_channel,by = c("id_channel"= "id_channel"))%>%
    select(name_channel,id_channel,id_video,ymd_hms_video)
  
  
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
  
  
  ### anti join (remove)
  
  videos <-
    videos%>%
    anti_join(tb_channel_to_rm, by = c("id_channel"="id_channel"))%>%
    anti_join(tb_video_to_rm,by =c("id_channel" = "id_channel","id_video"="id_video"))%>%
    left_join(tb_channel, by = c("name_channel" = "name_channel", "id_channel" ="id_channel"))
  
  tb_perf <- 
    tb_perf%>%
    anti_join(tb_channel_to_rm, by = c("id_channel"="id_channel"))%>%
    anti_join(tb_video_to_rm,by =c("id_channel" = "id_channel","id_video"="id_video"))
  
  
  
  
  
  videos_new <- 
    videos%>%
    anti_join(tb_perf,by = c("name_channel" = "name_channel", "id_channel" ="id_channel",
                             "id_video" = "id_video",    "ymd_hms_video" = "ymd_hms_video",
                             "id_twitter" = "id_twitter" ))
  
  
  
  
  
  
  ##### update new
  if(nrow(videos_new)>0L){
    message("Updating new videos")
    tb_perf_new <-
      videos_new%>%
      slice(seq_len(min(nrow(videos_new),credit)))%>%
        mutate(stats = map(id_video,tibble_video_stats))%>%
        unnest(stats)
    tb_perf_new <-
      tb_perf_new%>%
      mutate(url_channel = map_chr(id_channel,channel_url),
             url_video = map_chr(id_video,video_url),
             ymd_update = lubridate::today())
    
    tb_perf_new<-
      tb_perf_new%>%
      mutate(title_video = map_chr(url_video,function(url){
        message(glue("harvesting title: {url}"))
        Sys.sleep(runif(1,0,0.1))
        rvest::read_html(url)%>%
          html_elements("title")%>%
          html_text()
      }))
    
    tb_perf<- 
      tb_perf%>%
      bind_rows(tb_perf_new)
    
    credit = credit - nrow(tb_perf_new)
    
    
  }
  
  if(credit >0L){
    message("Updating 7 days old videos")
    
    out<- update_new_performance(tb_perf,
                                 min_days_update = 2,
                                 max_days_video = 7,
                                 credit = credit)
    
    tb_perf = out$data
    credit = out$credit
    
  }
  
  
  if(credit >0L){
    message("Updating 30 days old videos")
    
    out<- update_new_performance(tb_perf,
                                 min_days_update = 5,
                                 max_days_video = 30,
                                 credit = credit)
    
    tb_perf = out$data
    credit = out$credit
    
  }
  
  if(credit >0L){
    
    message("Updating oldest days old videos")
    out<- update_new_performance(tb_perf,
                                 min_days_update = 20,
                                 max_days_video = Inf,
                                 credit = credit,
                                 newest = F
                                 )
    
    tb_perf = out$data
    credit = out$credit
    
  }
  tb_perf
  
  }
  
  
  
  
  # if(credit >0L){
  #   
  #   tb_perf_update<- 
  #   tb_perf%>%
  #     arrange(ymd_update)%>%
  #     slice(seq_len(credit))%>%
  #     select(name_channel,id_channel,id_video,ymd_hms_video,id_twitter)%>%
  #     mutate(stats = map(id_video,tibble_video_stats))%>%
  #     unnest(stats)
  #   
  #   tb_perf_update <-
  #     tb_perf_update%>%
  #     mutate(url_channel = map_chr(id_channel,channel_url),
  #            url_video = map_chr(id_video,video_url),
  #            ymd_update = lubridate::today())
  #   
  #   tb_perf_update<-
  #     tb_perf_update%>%
  #     mutate(title_video = map_chr(url_video,function(url){
  #       message(glue("harvesting title: {url}"))
  #       Sys.sleep(runif(1,0,0.1))
  #       rvest::read_html(url)%>%
  #         html_elements("title")%>%
  #         html_text()
  #     }))
  #   
  #   tb_perf<- 
  #     tb_perf%>%
  #     filter(!(id_video%in%tb_perf_update$id_video))%>%
  #     bind_rows( tb_perf_update)
  #   
  #   
  #   
  #   
  # }
  

  #write_delim(tb_perf, file= file_perf, delim = ";")
  
  
  # }
  
  
  
update_new_performance <- function(data,
                                   min_days_update,
                                   max_days_video,
                                   credit,
                                   newest = T){
  

  
  tb_perf_update<-
    data%>%
    filter(today()-ymd_update>min_days_update,
           today()-as_date(ymd_hms_video)<max_days_video)
  
  
  if(nrow(tb_perf_update)>0L){
    
    used_credit = min(credit, nrow(tb_perf_update))
    
    if(newest){
    tb_perf_update<-
      tb_perf_update%>%
      arrange(desc(ymd_hms_video))
    }else{
      tb_perf_update<-
        tb_perf_update%>%
        arrange(ymd_update)
    }
    tb_perf_update<-
      tb_perf_update%>%
      slice(seq_len(credit))%>%
      select(name_channel,id_channel,id_video,ymd_hms_video,id_twitter)%>%
      mutate(stats = map(id_video,tibble_video_stats))%>%
      unnest(stats)
    
    credit = credit-used_credit
    
    tb_perf_update <-
      tb_perf_update%>%
      mutate(url_channel = map_chr(id_channel,channel_url),
             url_video = map_chr(id_video,video_url),
             ymd_update = lubridate::today())
    
    tb_perf_update<-
      tb_perf_update%>%
      mutate(title_video = map_chr(url_video,function(url){
        message(glue("harvesting title: {url}"))
        Sys.sleep(runif(1,0,0.1))
        rvest::read_html(url)%>%
          html_elements("title")%>%
          html_text()
      }))
    
    data<- 
      data%>%
      filter(!(id_video%in%tb_perf_update$id_video))%>%
      bind_rows( tb_perf_update)
    
  }
  
  return(list(data = data, credit = credit))
  
  
  
}

