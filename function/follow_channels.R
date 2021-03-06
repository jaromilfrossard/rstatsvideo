follow_channels <- function( file_channel = "data/list_channel.txt",silent=TRUE){
  
  
  tb_channels <- read_delim(file = file_channel,delim=";",
                            col_types = cols(
                              id_channel = col_character(),
                              name_channel = col_character(),
                              id_twitter = col_character()
                            ),lazy = F)
  
  
  
  
  following <- get_friends("rstatsvideo")
  
  tb_following <- 
    lookup_users(following$to_id)
  
  id_following <-
    tb_following %>%
    select(screen_name)
  
  new_following <- 
    tb_channels%>%
    select(id_twitter)%>%
    filter(!is.na(id_twitter))%>%
    anti_join(id_following, by = c("id_twitter" = "screen_name"))%>%
    pull(id_twitter)
  
  
  walk(new_following,\(x){
    if(!silent){message(x)}
    try(post_follow(x))})
  
}