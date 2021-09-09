post_videos <- function(id_channel, id_video, ymd_hms_video,tweet, file_tweet = "data/tweets.txt"){
  
  tb_tweet_old <-read_delim(file_tweet,delim=";",
                            col_types = cols(
                              id_channel = col_character(),
                              id_video = col_character(),
                              ymd_hms_video = col_datetime(),
                              tweet = col_character()
                            ))
  
  #rtweet::post_tweet(tweeti)
  #Sys.sleep(runif(1,25,40))
  tb_tweet_new <- tribble(~id_channel,~id_video,~ymd_hms_video,~tweet,
                          id_channel, id_video, ymd_hms_video,tweet )
  print(tb_tweet_new)
  tb_tweet_new <- 
    bind_rows(tb_tweet_old,
              tb_tweet_new)
  
  #write_delim(x = tb_tweet_new, file = file_tweet,delim=";")
  
  
}