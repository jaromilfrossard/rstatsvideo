load_current_video <- function(dir ="data/channels/"){
  if(stringr::str_sub(dir,-1,-1)!="/"){
    dir = paste0(dir,"/")
  }
  
  
  tibble(id_channel = list.files(dir))%>%
    mutate(videos = map(id_channel, function(id){
      read_delim(file = paste0(dir,id,"/video.txt"),delim=";",
                 col_types = cols(
                   id_video = col_character(),
                   ymd_hms_video = col_datetime()))%>%
        mutate(ymd_hms_video = ymd_hms(ymd_hms_video))
    }))%>%
    unnest(videos)%>%
    arrange(ymd_hms_video)
}