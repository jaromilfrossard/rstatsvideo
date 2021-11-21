tibble_video_infos = function(id_video){
  
  details = get_video_details(video_id = id_video )
  
  title = details$items[[1]]$snippet$title
  description = details$items[[1]]$snippet$description
  lang = details$items[[1]]$snippet$defaultAudioLanguage
  hashtag = unlist(details$items[[1]]$snippet$tags)
  
  if(is.null(title)){title = NA_character_}
  if(is.null(description)){description = NA_character_}
  if(is.null(lang)){lang = NA_character_}
  if(is.null(hashtag)){hashtag = NA_character_}
  
  
  #https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
  # if(is.na(lang)){
  #   clue = glue("{title} {description}")
  #   lang = textcat(clue)
  #   if(lang=="english"){
  #     lang="en"
  #   }else if(lang=="portuguese"){
  #     lang="pt"
  #   }else if(lang=="german"){
  #     lang="de"
  #   }else{
  #     message(glue("Language {lang} not recognized."))
  #     lang = NA_character_
  #   }
  # }
  
  
  tibble(title_video= title,
         description_video = description,
         lang_video = lang,
         hashtag = list(hashtag)
         )
}