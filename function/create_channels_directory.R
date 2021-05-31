##create the director of the channels id
create_channels_directory = function(id, dir = "data/channels/"){
  if(stringr::str_sub(dir,-1,-1)!="/"){
    dir = paste0(dir,"/")
  }
  

  path = paste0(dir,id)
  
  walk(.x =  path, function(pathi){
    if(!dir.exists(pathi)){
      dir.create(pathi)
    }
  })
}