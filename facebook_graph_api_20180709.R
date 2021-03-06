#### FACEBOOK GRAPH API ####
# Last modified : 2018.05.07

facebook_api <- function(token, keyword, type, limit,city) {
  
  # declare the empty objects
  data<-NULL
  data2<-NULL
  data3<-NULL
  data4<-NULL
  
  
  for (c in 1: nrow(city)){
    
  center<-paste0(city$telepules_lat[c],",",city$telepules_lon[c])  
  
  if (city$population[c] < 5000) {
    distance = 5000
  }
  else if (city$population[c] < 10000) {
    distance = 7000
  }
  else if (city$population[c] < 20000) {
    distance = 10000
  }
  else if (city$population[c] < 100000) {
    distance = 15000
  }
  else {
    distance = 20000
  }
    
  # cycle that iterates by keywords
  for (i in keyword) {       
    
  ## read URL
  adat <-try(
            fromJSON(
                      paste0(
                        "https://graph.facebook.com/v2.11/search?access_token=",token,
                        "&pretty=1&fields=name,single_line_address,phone,location,fan_count,overall_star_rating,price_range,category_list&q=",i,
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
  if (length(adat$data) == 0) next
    
    data<-cbind(
    data.table(adat$data$location),
    adat$data[,-which(names(adat$data)=="location")]
  )

  
  # additional information
  data$run_time<-Sys.time()
  data$type<-type
  data$category<-i
  
  #define paging URL
  url <- adat$paging$`next`
  
  
  while (!is.null(url)){
    
          adat2 <- fromJSON(url)
          
          
          data2<-cbind(
            data.table(adat2$data$location),
            adat2$data[,-which(names(adat2$data)=="location")]
          )
          
          data2$run_time<-Sys.time()
          data2$type<-type
          data2$category<-i
          
          url <- adat2$paging$`next`
          
          #binding data tables
          data <- rbindlist(list(data, data2), fill = T)
          
          #data$category<-i
          
          }
  
   data3<-rbindlist(list(data3,data),fill = T)
   data3<-distinct(data3)
  
  
  }
  data4<-rbindlist(list(data4,data3),fill = T)
  data4<-distinct(data4)
  
  
  
  }
  
  return(data4) 
   
}


