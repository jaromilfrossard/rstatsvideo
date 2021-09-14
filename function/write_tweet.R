write_tweet <- function(name_channel, id_twitter, id_video, new_video){
  
  tb_infos <- tibble_video_infos(id_video)
  
  tweet_header <- write_tweet_header(title_video = tb_infos$title_video, new_video = new_video)
  
  tweet_author <- write_tweet_autor(name_channel, id_twitter)
  
  tweet_lang = write_tweet_lang(as.character(tb_infos$lang_video))
  
  tweet_url <- write_tweet_url(id_video)
  
  tweet_lines <- c(tweet_header,tweet_author,tweet_lang,tweet_url)
  tweet_lines <- as.character(na.omit(tweet_lines))
  
  paste0(tweet_lines,collapse = "\n")
  
}


write_tweet_header <- function(title_video,new_video){
  if(new_video){
    glue("\U0001f4fa New #rstats video: {title_video}")
  }else{
    glue("\U0001f4fa #rstats video: {title_video}")
  }
}


write_tweet_autor <- function(name_channel, id_twitter = NULL){
  if(!is.na(id_twitter)){
    glue("\U0025b6 {name_channel} (@{id_twitter})")
  }else{
    glue("\U0025b6 {name_channel}")
  }
}


write_tweet_url <- function(id_video){
  glue("\U0001f517 {video_url(id_video)}")
}


write_tweet_lang <- function(lang){
  
  if(is.na(lang)){
    NA_character_
  }else if(lang=="fr"){
    glue("\U001F5E3 \U001F1EB\U001F1F7")
  }else if(lang %in% c("en","en-US","en-GB","en-CA")){
    glue("\U001F5E3 \U001F1EC\U001F1E7")
  }else if(lang =="de"){
    glue("\U001F5E3 \U001F1E9\U001F1EA")
  }else if(lang ==c("es","es-419")){
    glue("\U001F5E3 \U001F1EA\U001F1F8")
  }else if(lang=="zxx"){
    NA_character_
  }else{
    message(glue("Language {lang} not recognized."))
    NA_character_
  }
}
