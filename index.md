---
title: "rstatsvideo"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---




<!-- https://www.r-bloggers.com/2020/09/deploying-flexdashboard-on-github-pages/ -->


# Channels

<!-- Column {data-width=650} -->
<!-- ----------------------------------------------------------------------- -->

### Explore Youtube Channels





```r
tb_perf_channel%>%
  DT::datatable(escape = FALSE,
                filter = list(position = 'top', clear = FALSE),
                options = list(
    search = list( caseInsensitive = TRUE),
    pageLength = 20
  ),
  caption = glue("Date: {date_update}"))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```



# Videos

Column {.tabset}
-------------------------------------


### All Youtube Videos by channel




```r
gg<-
  tb_perf%>%
  mutate(name_channel= factor(name_channel,ordered = T),
         name_channel=fct_rev(name_channel))%>%
         #,
         #name_channel = reorder(name_channel,name_channel,length)
         #)%>%
  ggplot(aes(x = ymd_hms_video, y = name_channel,color = name_channel))+
  geom_point_interactive(aes(tooltip = title_video,
                             onclick = paste0('window.open("', url_video , '")'),
                             data_id = id_video),
                         show.legend = F)+
  labs(title = "All #rstats videos - Click to access the video", 
       x = "Release date",y = "")+
  #scale_y_reverse()+
  theme_bw()+
  theme(axis.text.y = element_text(size=2),
        axis.text.x = element_text(size=4),
        axis.title.x = element_text(size=5),
        panel.grid.minor.y = element_line(size=1))
girafe(code = print(gg),
       pointsize = 1,
       options = list(
         opts_sizing(rescale = TRUE, width = 1)),
       height_svg = 2.4)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

### Table of all Youtube Videos



```r
tb_perf_video%>%
  DT::datatable(escape = FALSE,
                filter = list(position = 'top', clear = FALSE),
                options = list(
    search = list( caseInsensitive = TRUE),
    pageLength = 20
  ),
  caption = glue("Date: {date_update}"))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

