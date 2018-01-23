#### FACEBOOK GRAPH API ####

facebook_api <- function(token, keyword, type, limit,center,distance) {
  
  data3<-NULL
  
  for (i in seq_len(length(keyword))) {
    
    
  ## read URL
  adat <-
    fromJSON(
      paste0(
        "https://graph.facebook.com/v2.10/search?access_token=",token,
        "&pretty=1&fields=name%2Csingle_line_address%2Cphone%2Clocation%2Cfan_count&q=",keyword[i],
        "&center=",center,
        "&distance=",distance,
        "&type=",type,
        "&limit=",limit,
        "&after=MjYZD"
      )
    )
  # transform to data.table
  data<-cbind(
              data.table(adat$data$location),
              adat$data[,-which(names(adat$data)=="location")]
              )
  
  data$run_time<-Sys.time()
  data$type<-type
  data$category<-keyword[i]
  
  #define paging URL
  url <- adat$paging$`next`
  
  
  while (!is.null(url)){
    
          adat <- fromJSON(url)
          data2<-cbind(
                       data.table(adat$data$location),
                       adat$data[,-which(names(adat$data)=="location")]
                      )
          
          data2$run_time<-Sys.time()
          data2$type<-type
          data2$category<-keyword[i]
          
          url <- adat$paging$`next`
          
          #binding data tables
          data <- rbindlist(list(data, data2), fill = T)
          
          }
  
  data3<-rbindlist(list(data3,data),fill = T)
  
  }
  
  return(data3)
  
}


