tibble_video_infos = function(id_video){
  
  details = get_video_details(video_id = id_video )
  
  title = details$items[[1]]$snippet$title
  description = details$items[[1]]$snippet$description
  lan = details$items[[1]]$snippet$defaultAudioLanguage
  if(is.null(title)){title = NA_character_}
  if(is.null(description)){description = NA_character_}
  if(is.null(lan)){lan = NA_character_}
  
  tibble(title_video= title,
         description_video = description,
         lan_video = lan
         )
}