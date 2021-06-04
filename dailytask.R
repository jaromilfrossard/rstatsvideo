gert::git_pull()

tb_channel <- readr::read_delim("data/list_channel.txt",delim=";",
                                col_types = cols(
                                  id_channel = col_character(),
                                  name_channel = col_character()))

#update channels videos
walk(tb_channel$id_channel,update_channel_video)


##tweet_new videos
tweet_videos(tweet_older = T)

commit_message <- paste0("update ",format(Sys.Date()+1, "%Y_%m_%d"))
gert::git_commit_all(commit_message)

gert::git_push()