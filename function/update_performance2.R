# file_channel = "data/list_channel.txt";
# file_perf = "data/performance.csv";
# file_channel_to_rm = "data/list_channel_to_rm.txt";
# file_video_to_rm = "data/list_video_to_rm.txt";
# credit = 10;




update_performance2 <- function(credit = 200,
                                file_channel = "data/list_channel.txt",
                                file_perf = "data/performance.csv",
                                file_channel_to_rm = "data/list_channel_to_rm.txt",
                                file_video_to_rm = "data/list_video_to_rm.txt"){
  
  
  
  
  
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
  
  
  videos
  
  tb_perf_new <- 
    videos%>%
    anti_join(tb_perf,by = c("name_channel" = "name_channel", "id_channel" ="id_channel",
                             "id_video" = "id_video",    "ymd_hms_video" = "ymd_hms_video",
                             "id_twitter" = "id_twitter" ))%>%
    mutate(count_view = NA_integer_,
           count_comment = NA_integer_,
           url_channel = channel_url(id_channel),
           url_video = video_url(id_video),
           ymd_update = NA_Date_,
           title_video = NA_character_)
  
  tb_perf <- bind_rows(tb_perf,tb_perf_new)
  
  
  #### order and select videos
  tb_perf_update<-
    tb_perf%>%
    mutate(ratio = ratio_date(ymd_update,date(ymd_hms_video)))%>%
    arrange(desc(ratio),!is.na(ymd_update))%>%
    slice(seq_len(credit))
  

  rr <- range(tb_perf_update$ratio)
  message(glue("Updating ration from {round(rr[2],2)} to {round(rr[1],2)}"))
  ### updating stats
  
  tb_perf_update<-
    tb_perf_update%>%
    select(name_channel,id_channel,id_video,ymd_hms_video,id_twitter)%>%
    mutate(stats = map(id_video,tibble_video_stats))%>%
    unnest(stats)
  
  ###updating date
  
  tb_perf_update <-
    tb_perf_update%>%
    mutate(url_channel = map_chr(id_channel,channel_url),
           url_video = map_chr(id_video,video_url),
           ymd_update = lubridate::today())
  
  
  ####updating title
  
  tb_perf_update<-
    tb_perf_update%>%
    mutate(title_video = map_chr(url_video,function(url){
      message(glue("harvesting title: {url}"))
      Sys.sleep(runif(1,0,0.1))
      rvest::read_html(url)%>%
        html_elements("title")%>%
        html_text()
    }))
  
  
  tb_perf<- 
    tb_perf%>%
    filter(!(id_video%in%tb_perf_update$id_video))%>%
    bind_rows(tb_perf_update)
  
  tb_perf

  
}


ratio_date <- function(date_update, date_release){
  diff_update  <- as.numeric(today()-date_update)
  diff_release <- as.numeric(today()-date_release)
  
  ratio <- diff_update/diff_release
  ratio <- ifelse(diff_update==0,0,ratio)
  ratio <- ifelse(is.na(ratio),Inf,ratio)
  ratio
}



