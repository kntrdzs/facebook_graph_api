#### FACEBOOK GRAPH API ####
#https://graph.facebook.com/v2.8/search?access_token=EAACEdEose0cBACn6ZBlet9dqwfIBWi132MM8YuYHTwZCvAYa4v4S4t7ph1ZAWBtx2ycfZARm8z47ZAKiX0vNgepRCqt2hcZCEGcn4ZBEdYNTFOAZAvLKY5O8a8ZCDy6IiMxeOhkv5GYsHXl1ZAhwRbcn99YfM0KgCVPGmwvevTZCGTAklB1tGAUg7bXmUzUzB36ZAkjcW5aC1C5jOAZDZD&pretty=1&fields=name,single_line_address,phone,location,fan_count,overall_star_rating,price_range,link&q=alapitvany&center=47.49801,19.03991&distance=20000&type=page&limit=300&after=MjYZD

facebook_api <- function(token, keyword, type, limit,center,distance) {
  
  # declare the empty objects
  data<-NULL
  data2<-NULL
  data3<-NULL
  
  # cycle that iterates by keywords
  for (i in seq_len(length(keyword))) {       
    
  ## read URL
  adat <-try(
            fromJSON(
                      paste0(
                        "https://graph.facebook.com/v2.11/search?access_token=",token,
                        "&pretty=1&fields=name,single_line_address,phone,location,fan_count,overall_star_rating,price_range&q=",keyword[1],
                        "&center=",center,
                        "&distance=",distance,
                        "&type=",type,
                        "&limit=",limit, 
                        "&after=MjYZD"
                      )
                    ),
            silent=T
            )
  
  if (is(adat,"try-error")) next
  
  
  adat_2<-adat$data
  
  # list to data.frame
  for (i in 1:length(adat_2))  {
    
    one_line<-data.frame(adat_2[[i]])
    data<-rbindlist(list(data,one_line), fill=T)
    
    
  }

  
  # additional information
  data$run_time<-Sys.time()
  data$type<-type
  data$category<-keyword[i]
  
  #define paging URL
  url <- adat$paging$`next`
  
  
  while (!is.null(url)){
    
          adat2 <- fromJSON(url)
          
          
          
          adat2_2<-adat2$data
          
          # 
          for (j in 1:length(adat2_2))  {
            
            one_line2<-data.frame(adat2_2[[j]])
            data2<-rbindlist(list(data2,one_line2), fill=T)
            
            
          }
          
          data2$run_time<-Sys.time()
          data2$type<-type
          data2$category<-keyword[i]
          
          url <- adat2$paging$`next`
          
          #binding data tables
          data <- rbindlist(list(data, data2), fill = T)
          
          }
  
  data3<-rbindlist(list(data3,data),fill = T)
  
  }
  
  return(data3) 
  
}


