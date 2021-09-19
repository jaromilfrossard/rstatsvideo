check_videos <- function(tb_videos,file_video_to_rm = "data/list_video_to_rm.txt"){
  
  i = 1
  checking = "continue"
  while(checking=="continue"){
    
    urli <- video_url(tb_videos$id_video[i])
    browseURL(urli)
    res <- svDialogs::dlg_list(c("accept","reject","stop"))
    
    if(res$res=="reject"){
      message(glue("{i} of {nrow(tb_videos)}: {urli} : rejected!"))
      
      tb_video_to_rm <- read_delim(file_video_to_rm,delim=";",
                                   col_types = cols(
                                     id_channel = col_character(),
                                     id_video = col_character()
                                   ),
                                   lazy = FALSE)
      
      tb_video_to_rm%>%
        bind_rows(
          tribble(~id_channel,~id_video,
                  tb_videos$id_channel[i],tb_videos$id_video[i]))%>%
        write_delim(x =., file = file_video_to_rm,delim=";")
    }else if(res$res=="accept"){
      message(glue("{i} of {nrow(tb_videos)}: {urli} : accepted!"))
    }else if(res$res=="stop"){
      print(as_date(tb_videos$ymd_hms_video[i])-1)
      checking="stop"}
    
    if(i>=nrow(tb_videos)){
      checking="stop"
    }
    
    i = i+1
    
  }
  
  
  
}