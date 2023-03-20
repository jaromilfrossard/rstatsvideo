standardise_language <- function(x){
  
  x <- as.character(x)
  
  
  tb <- tribble(~conditions,~output,
    c("ar"),"ar",
    c("de","de-CH"),"de",
    c("en","en-US","en-GB","en-CA","en-IN"),"en",
    c("es","es-419","es-MX"),"es",
    c("fa","fa-IR"),"fa",
    c("fr","fr-FR"), "fr",
    c("gl"),"gl",
    c("hi"),"hi",
    c("it"),"it",
    c("ko"),"ko",
    c("ku"),"ku",
    c("pt", "pt-BR"),"pt",
    c("ru"),"ru",
    c("tr"),"tr",
    c("vi"),"vi",
    c("yo"),"yo",
    c("zh"),"zh",
    c("zh-CN"),"zh_cn",
    c("zxx"),NA_character_)
  
  
  unknown <- which(!(x%in%unlist(tb$conditions))&(!is.na(x)))
  
  if(length(unknown)>0){
    message(glue("Language {paste0(x[unknown],collapse=', ')} not recognized."))
    x[unknown]<-NA_character_
  }
  
  
  for(i in seq_len(nrow(tb))){
    x <- ifelse(x%in%(tb$conditions[[i]]),tb$output[i],x)
  }
  

  x

}



language2emoji <- function(x){
  
  x <- as.character(x)
  
  tb <- tribble(~lang,~emoji,
                "ar","Arabic",
                "de","\U001F1E9\U001F1EA",
                "es","\U001F1EA\U001F1F8",
                "en","\U001F1EC\U001F1E7",
                "fa","\U001F1EE\U001F1F7",
                "fr","\U001F1EB\U001F1F7",
                "gl","Galician",
                "hi","\U001F1EE\U001F1F3",
                "it","\U001F1EE\U001F1F9",
                "ko","\U001F1F0\U001F1F7",
                "ku","Kurdish",
                "pt","\U001F1F5\U001F1F9",
                "ru","\U001F1F7\U001F1FA",
                "tr","\U001F1F9\U001F1F7",
                "vi","\U001F1FB\U001F1F3",
                "yo","Yoruba",
                "zh","\U001F1E8\U001F1F3",
                "zh_cn","\U001F1E8\U001F1F3")
  
  unknown <- which(!(x%in%tb$lang)&(!is.na(x)))
  if(length(unknown)>0){
    message(glue("No emoji for languge {paste0(x[unknown],collapse=', ')}."))
    x[unknown]<-NA_character_
  }
  
  for(i in seq_len(nrow(tb))){
    x <- ifelse(x%in%(tb$lang[i]),tb$emoji[i],x)
  }
  

  x
  
}
