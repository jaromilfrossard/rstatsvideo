write_tweet_txt = function(id_video, title_video, hashtags = NULL, header = "New #rstats video:", name_channel = NULL){
  
  if(!is.null(hashtags)){
    hashtags = sapply(hashtags,function(hi){
      if(stringr::str_sub(hi,1,1)!="#"){
        hi = paste0("#",hi)}
      hi})
    hashtags = paste(hashtags, collapse = " ")
  
  }else{
    hashtags = ""
  }
  
  if(is.null(name_channel)){
    tweet = glue("{header} {title_video}\n{video_url(id_video)} {hashtags}")}
  else{
    tweet = glue("{header} {title_video}\nBy {name_channel}\n{video_url(id_video)} {hashtags}") 
    }
  str_trim(tweet)
   
}



