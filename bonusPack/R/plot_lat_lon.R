#' @author mahnaz
#' @concept Handling large datasets with dplyr
#' @description visualize_airport_delays
#' @import nycflights13
#' @import dplyr
#' @import ggplot2


visualize_airport_delays <- function(){
  flight <- dplyr::select(nycflights13::flights, dep_delay,arr_delay,origin)
  airport <- dplyr::select(nycflights13::airports, faa,name,lat,lon)
  colnames(airport) <- c("origin","name","lat","lon")
  deplay <- dplyr::left_join(flight,airport, by = "origin")
  deplay$sum_delay <- deplay$dep_delay+deplay$arr_delay
  data <- dplyr::group_by(deplay, name)
  data1 <- dplyr::mutate(data,mean_delay = mean(sum_delay, na.rm = TRUE))
  #data <- deplay %>% group_by(name) %>% mutate(mean_delay = mean(sum_delay, na.rm = TRUE))
  
  gp <- ggplot2::ggplot(data,ggplot2::aes(x=data1$lat , y= data1$lon))+ggplot2::geom_point(shape=1,size=data1$mean_delay)+
    ggplot2::geom_text(ggplot2::aes(label = data$name))+ggplot2::xlim(40,42)+ggplot2::ylim(-74.5,-73.6)+
    ggplot2::labs(x = "Latitude",
         y = "Longitude",title = "airport_delays") 
  return(gp)
  
}

#visualize_airport_delays()





