tibble_video_stats = function(id_video){
  
  message(glue("getting stats from: {id_video}" ))
  
  out = get_stats(id_video)
  if(length(out)==0L){
    tibble(count_view=NA_integer_,
           count_comment=NA_integer_)
  }else{
    count_view = out$viewCount
    count_comment = out$commentCount
    if(is.null(count_view)){count_view=0L}
    if(is.null(count_comment)){count_comment=0L}
    tibble(count_view=as.integer(count_view),
           count_comment=as.integer(count_comment))
  }
  

  
}