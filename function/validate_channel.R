validate_channel = function(id_channel,
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
  
  tb_videos%>%
  filter(id_channel==id_channel)%>%
  check_videos()
  
  
  

  
}