#### FACEBOOK GRAPH API ####
#https://graph.facebook.com/v2.8/search?access_token=EAACEdEose0cBACn6ZBlet9dqwfIBWi132MM8YuYHTwZCvAYa4v4S4t7ph1ZAWBtx2ycfZARm8z47ZAKiX0vNgepRCqt2hcZCEGcn4ZBEdYNTFOAZAvLKY5O8a8ZCDy6IiMxeOhkv5GYsHXl1ZAhwRbcn99YfM0KgCVPGmwvevTZCGTAklB1tGAUg7bXmUzUzB36ZAkjcW5aC1C5jOAZDZD&pretty=1&fields=name,single_line_address,phone,location,fan_count,overall_star_rating,price_range,link&q=alapitvany&center=47.49801,19.03991&distance=20000&type=page&limit=300&after=MjYZD

facebook_api <- function(token, keyword, type, limit,center,distance) {
  
  data3<-NULL
  
  for (i in seq_len(length(keyword))) {
    
    
  ## read URL
  adat <-try(
    fromJSON(
      paste0(
        "https://graph.facebook.com/v3.0/search?access_token=",token,
        "&pretty=1&fields=name,single_line_address,phone,location,fan_count,overall_star_rating,price_range&q=",keyword[i],
        "&center=",center,
        "&distance=",distance,
        "&type=",type,
        "&limit=",limit,
        "&after=MjYZD"
      )
    ),
    silent=T)
  
  if (is(adat,"try-error")) next
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


