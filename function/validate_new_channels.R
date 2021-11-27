validate_new_channels <- function(dir = "data/channels"){
  new_channels = tb_channel%>%
    filter(!(id_channel%in%list.files(dir)))%>%
    pull(id_channel)
  
  if(length(new_channels>0)){
    message(glue('Channels to update and validate:\n{glue_collapse(new_channels,sep = "\n")}'))
    
    for(i in seq_along(new_channels)){
      message(glue("Channel: {new_channels[i]}"))
      update_channel_video(id = new_channels[i])
      validate_channel(new_channels[i]) 
    }
    
  }
  
}