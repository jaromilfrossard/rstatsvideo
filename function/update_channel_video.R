update_channel_video = function(id, dir = "data/channels",max_results = 2000){
  if(stringr::str_sub(dir,-1,-1)!="/"){
    dir = paste0(dir,"/")
  }
  
  if(!dir.exists(paste0(dir,id))){
    create_channels_directory(id = id,dir = dir)
  }
  
  if(!file.exists(paste0(dir,id,"/video.txt"))){
    txt = "id_video;ymd_hms_video"
    write_lines(x = txt,file = paste0(dir,id,"/video.txt"))
  }
  
  tb_videos_current <- read_delim(file = paste0(dir,id,"/video.txt"),delim=";",
             col_types = cols(
               id_video = col_character(),
               ymd_hms_video = col_datetime()))%>%
    mutate(ymd_hms_video = ymd_hms(ymd_hms_video))
  
  
  tb_videos_new = list_channel_videos(channel_id = id,
                                    max_results = max_results)
  
  #check empty channel
    if(!is.null(tb_videos_new$contentDetails.videoPublishedAt)){
      tb_videos_new <-
        tb_videos_new%>%
        transmute(id_video = contentDetails.videoId, 
                  ymd_hms_video = ymd_hms(contentDetails.videoPublishedAt))
      
      ### check date
      if(nrow(tb_videos_current)>0&nrow(tb_videos_new)>max_results){
        warnings("Current number of videos = 0 and new videos = max_results. Increase max_results.")
      }
      
      tb_videos_new<- 
        tb_videos_new%>%
        filter(!id_video%in%tb_videos_current$id_video)
      
      
      if(nrow(tb_videos_new)>max_results){
        warnings("New ADDITIONAL videos = max_results. Increase max_results.")
      }
      
      
      
      
      message(glue("channel id: {id}. Adding {nrow(tb_videos_new)} new to {nrow(tb_videos_current)} current videos"))
      bind_rows(tb_videos_current,tb_videos_new)%>%
        arrange(ymd_hms_video)%>%
        write_delim(x =., file = paste0(dir,id,"/video.txt"),delim=";")
      
    }
      
}
  