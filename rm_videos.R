id_channel ="UClHxi-ei0TXjC-0A0QXwMpg"


video <- tuber::list_channel_videos(id_channel,max_results = 5000)

tb_video_to_rm <- read_delim("data/list_video_to_rm.txt",delim=";",
                             col_types = cols(
                               id_channel = col_character(),
                               id_video = col_character()
                             ),
                             lazy = FALSE)

video_sample<- 
  video%>%
  as_tibble()%>%
  transmute(id_channel = id_channel,
            id_video = contentDetails.videoId,
            date = as_date(contentDetails.videoPublishedAt))%>%
  anti_join(tb_video_to_rm,by ="id_video")%>%
  arrange(date)

video_sample<-
video_sample%>%
  mutate(url_video=video_url(id_video))%>%
  mutate(title_video = map_chr(url_video,function(url){
    Sys.sleep(runif(1,0,0.01))
    rvest::read_html(url)%>%
      html_elements("title")%>%
      html_text()
  }))


video_sample%>%
  filter(!(str_detect(title_video,fixed("R"))|str_detect(title_video,fixed("RStudio"))))%>%
  mutate(csv = glue("{id_channel};{id_video}"))%>%
  select(title_video,csv)%>%
  select(title_video)%>%
  print(n=2000)



video_sample%>%
  filter(!(str_detect(title_video,fixed("R"))|str_detect(title_video,fixed("RStudio"))))%>%
  mutate(csv = glue("{id_channel};{id_video}"))%>%
  select(title_video,csv)%>%
  pull(csv)%>%{.[1201:1342]}
