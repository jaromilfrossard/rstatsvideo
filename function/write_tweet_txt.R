write_tweet_txt = function(id_video, title_video, hashtags = NULL, header = "New #rstats video:"){
  
  if(!is.null(hashtags)){
    hashtags = sapply(hashtags,function(hi){
      if(stringr::str_sub(hi,1,1)!="#"){
        hi = paste0("#",hi)}
      hi})
    hashtags = paste(hashtags, collapse = " ")
  
  }else{
    hashtags = ""
  }

  tweet = glue("{header} {title_video} {video_url(id_video)} {hashtags}")
  str_trim(tweet)
   
}



