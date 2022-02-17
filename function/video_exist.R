
#id_video = "zlPLV4-FS8g"
video_exist <- function(id_video){
  url <- video_url(id_video)
  page <- rvest::read_html(url)

}