write_tweet <- function(name_channel, id_twitter, id_video, new_video){
  
  tb_infos <- tibble_video_infos(id_video)
  
  tweet_header <- write_tweet_header(title_video = tb_infos$title_video, new_video = new_video)
  
  tweet_author <- write_tweet_autor(name_channel, id_twitter)
  
  tweet_lang <- write_tweet_lang(as.character(tb_infos$lang_video))
  
  tweet_url <- write_tweet_url(id_video)
  
  
  current_len = sum(na.omit(nchar(c(tweet_header,tweet_author,tweet_lang,tweet_url),type = "width")+2))
  tweet_hashtag = write_tweet_hashtag(tb_infos$hashtag[[1]],280-current_len)
  
  tweet_lines <- c(tweet_header,tweet_author,tweet_lang,tweet_hashtag,tweet_url)
  tweet_lines <- as.character(na.omit(tweet_lines))
  
  tweet <- paste0(tweet_lines,collapse = "\n")
  
  if(nchar(tweet,type = "width")>280L){
    stop(glue("tweet to long {nchar(tweet,type = 'width')}"))
  }
  tweet
  
}


write_tweet_header <- function(title_video,new_video){
  if(new_video){
    glue("\U0001f4fa New #rstatsvideo: {title_video}")
  }else{
    glue("\U0001f4fa #rstatsvideo: {title_video}")
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
  #https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
  
  if(is.na(lang)){
    NA_character_
  }else if(lang%in%c("fr","fr-FR")){
    glue("\U001F5E3 \U001F1EB\U001F1F7")
  }else if(lang %in% c("en","en-US","en-GB","en-CA")){
    glue("\U001F5E3 \U001F1EC\U001F1E7")
  }else if(lang %in%"de"){
    glue("\U001F5E3 \U001F1E9\U001F1EA")
  }else if(lang %in%c("pt", "pt-BR")){
    glue("\U001F5E3  \U001F1F5\U001F1F9")
  }else if(lang %in%"tr"){
    glue("\U001F5E3 \U001F1F9\U001F1F7")
  }else if(lang %in%"it"){
    glue("\U001F5E3 \U001F1EE\U001F1F9")
  }else if(lang %in%"ko"){
    glue("\U001F5E3 \U001F1F0\U001F1F7")
  }else if(lang %in%"yo"){
    glue("\U001F5E3 Yoruba")
  }else if(lang %in%c("es","es-419")){
    glue("\U001F5E3 \U001F1EA\U001F1F8")
  }else if(lang%in%"zxx"){
    NA_character_
  }else{
    message(glue("Language {lang} not recognized."))
    NA_character_
  }
}


write_tweet_hashtag <- function(hashtag, max_len){
  hi <- 
    hashtag%>%
    str_remove_all("[[:punct:]]")%>%
    str_remove_all(fixed(" "))%>%
    tolower()
  hi <- c("rstats",hi)
  
  #hi <- hi[!(hi%in%c("rstats",""))]
  hi <- paste0("#", na.omit(hi))
  hi <- unique(hi)
  hi <- paste(hi[cumsum(nchar(hi,"width")+1)<(max_len+2)],collapse = " ")

  if(hi=="#"){
    hi = NA_character_
  }
  
  return(hi)

}

