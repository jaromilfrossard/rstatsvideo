standardise_language <- function(x){
  
  tb <- tribble(~conditions,~output,
    c("fr","fr-FR"), "fr",
    c("en","en-US","en-GB","en-CA"),"en",
    c("de"),"de",
    c("pt", "pt-BR"),"pt",
    c("tr"),"tr",
    c("it"),"it",
    c("ko"),"ko",
    c("ru"),"ru",
    c("yo"),"yo",
    c("es","es-419","es-MX"),"es",
    c("fa"),"fa",
    c("zxx"),NA_character_)
  
  x <- ifelse(is.na(x),NA_character_,x)
  for(i in seq_len(nrow(tb))){
    x <- ifelse(x%in%(tb$conditions[[i]]),tb$output[i],x)
  }
  
  unknown <- which(!(x%in%unlist(tb$conditions))&(!is.na(x)))
  if(length(unknown)>0){
    message(glue("Language {paste0(x[unknown],collapse=', ')} not recognized."))
    x[unknown]<-NA_character_
    
  }
  x

}



language2emoji <- function(x){
  tb <- tribble(~lang,~emoji,
                "fr","\U001F1EB\U001F1F7",
                "en","\U001F1EC\U001F1E7",
                "de","\U001F1E9\U001F1EA",
                "pt","\U001F1F5\U001F1F9",
                "tr","\U001F1F9\U001F1F7",
                "it","\U001F1EE\U001F1F9",
                "ko","\U001F1F0\U001F1F7",
                "ru","\U001F1F7\U001F1FA",
                "yo","Yoruba",
                "es","\U001F1EA\U001F1F8",
                "fa","\U001F1EE\U001F1F7")
  
  
  x <- ifelse(is.na(x),NA_character_,x)
  for(i in seq_len(nrow(tb))){
    x <- ifelse(x%in%(tb$lang[i]),tb$emoji[i],x)
  }
  
  unknown <- which(!(x%in%tb$emoji)&(!is.na(x)))
  if(length(unknown)>0){
    message(glue("No emoji for languge {paste0(x[unknown],collapse=', ')}."))
    x[unknown]<-NA_character_
  }
  x
  
}