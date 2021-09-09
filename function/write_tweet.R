write_tweet <- function(name_channel, id_twitter, id_video, new_video){
  
  tb_infos <- tibble_video_infos(id_video)
  
  tweet_header <- write_tweet_header(title_video = tb_infos$title_video, new_video = new_video)
  
  tweet_author <- write_tweet_autor(name_channel, id_twitter)
  
  tweet_url <- write_tweet_url(id_video)
  
  glue("{tweet_header}\n{tweet_author}\n{tweet_url}")
  
}


write_tweet_header <- function(title_video,new_video){
  if(new_video){
    glue("\U0001f4fa New #rstats video: {title_video}")
  }else{
    glue("\U0001f4fa #rstats video: {title_video}")
  }
}


write_tweet_autor <- function(name_channel, id_twitter = NULL){
  if(!is.null(id_twitter)){
    glue("\U0001f517 {name_channel}(@{id_twitter})")
  }else{
    glue("\U0001f517 {name_channel}")
  }
}


write_tweet_url <- function(id_video){
  glue("\U0001f517 {video_url(id_video)}")
}