#### FACEBOOK GRAPH API ####

facebook_api <- function(token, keyword, type, limit) {
  
  
 # for (i in seq_len(length(keyword))) {
    
    
  ## read URL
  adat <-
    fromJSON(
      paste0(
        "https://graph.facebook.com/v2.10/search?access_token=",
        token,
        "&pretty=1&fields=name%2Csingle_line_address%2Cphone%2Clocation%2Cfan_count&q=",
        keyword,
        "&type=",
        type,
        "&limit=",
        limit,
        "&after=MjYZD"
      )
    )
  # transform to data.table
  data<-cbind(data.table(adat$data$location),adat$data[,-which(names(adat$data)=="location")])
  
  data$category<-keyword
  #define paging URL
  url <- adat$paging$`next`
  
  
  while (!is.null(url)){
    
          adat <- fromJSON(url)
          data2<-cbind(data.table(adat$data$location),adat$data[,-which(names(adat$data)=="location")])
          
          data2$category<-keyword
          
          url <- adat$paging$`next`
          
          #binding data tables
          data <- rbindlist(list(data, data2), fill = T)
          
        }
  #}
  
  return(data)
  
}

