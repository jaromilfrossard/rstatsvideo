channel_exist = function(id_channel){
  !is.null(tuber:::tuber_GET("channels", list(id = id_channel))$items)
}
