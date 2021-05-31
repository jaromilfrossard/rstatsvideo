list_channel_activities_jf <- function (filter = NULL, part = "snippet", max_results = 50, 
          page_token = NULL, published_after = NULL, published_before = NULL, 
          region_code = NULL, simplify = TRUE, ...) 
{
  if (max_results < 0) {
    stop("max_results only takes a value between 0 and 50.")
  }
  if (!(names(filter) %in% c("channel_id"))) {
    stop("filter can only take one of values: channel_id.")
  }
  if (length(filter) != 1) 
    stop("filter must be a vector of length 1.")
  if (is.character(published_after)) {
    if (is.na(as.POSIXct(published_after, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("The date is not properly formatted in RFC 339 Format.")
    }
  }
  if (is.character(published_before)) {
    if (is.na(as.POSIXct(published_before, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("The date is not properly formatted in RFC 339 Format.")
    }
  }
  translate_filter <- c(channel_id = "channelId")
  yt_filter_name <- as.vector(translate_filter[match(names(filter), 
                                                     names(translate_filter))])
  names(filter) <- yt_filter_name
  querylist <- list(part = part, maxResults = max_results, 
                    pageToken = page_token, publishedAfter = published_after, 
                    publishedBefore = published_before, regionCode = region_code)
  querylist <- c(querylist, filter)
  raw_res <- tuber:::tuber_GET("activities", querylist, ...)
  if (length(raw_res$items) == 0) {
    warning("No comment information available. Likely cause: Incorrect ID.\n")
    if (simplify == TRUE) 
      return(data.frame())
    return(list())
  }
  if (simplify == TRUE & part == "snippet") {
    simple_res <- lapply(raw_res$items, function(x) unlist(x$snippet))
    simpler_res <- plyr:::ldply(simple_res, rbind)
    return(simpler_res)
  }
  raw_res
}