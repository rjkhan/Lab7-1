#' A function which data munges nycflights13 and plots the mean arrival delays
#' of airports.
#' 
#' This function data munges nycflights13 and generates a plot with mean arrival
#' delays at different airports.
#' 
#' @return p A ggplot object of mean arrival delays of flights from New York to
#'   cities outside NYC.
#' @export 


visualize_airport_delays <- function(){
  library(dplyr)
  library(ggplot2)
  library(mapproj)
  library(plyr)
  library(nycflights13)
  
  a <- select(flights, one_of(c("dep_delay", "arr_delay", "origin", "dest")))
  b <- select(airports, one_of(c("faa", "lat", "lon")))
  k <- filter(b,faa %in% c(unique(a$dest)))
  jfk <- filter(a, origin == "JFK" & !is.na(dep_delay))
  ewr <- filter(a, origin == "EWR" & !is.na(dep_delay))
  lga <- filter(a, origin == "LGA" & !is.na(dep_delay))
  dep_jfk <- summarise(jfk, dep_jfk = mean(dep_delay))
  dep_ewr <- summarise(ewr, dep_ewr = mean(dep_delay))
  dep_lga <- summarise(lga, dep_lga = mean(dep_delay))
  
  arrivaldelays <- select(flights, one_of(c("arr_delay", "dest")))
  arrivaldelays <- filter(arrivaldelays, !is.na(arr_delay)) 
  
  arrivaldelays4 <- dplyr::summarise(dplyr::group_by(arrivaldelays, dest),  avg=mean(arr_delay, na.rm = TRUE))
  
  # it seems like in the data with latitude and longitude there aren't some airports that can be found in arrivaldelays
  k1 <- rename(k, replace = c("faa" = "dest"))
  papa <- inner_join(k1,  arrivaldelays4, by = "dest")
  
  all_states <- map_data("state")
  
  
  p <- ggplot(papa, aes(x = lon, y = lat)) +
    geom_text(aes(label = signif(avg,2)), size = 5) +
    
    labs(x = "Longitude deg.", y = "Latitude deg.") +
    ggtitle("mean Airport arrival delays, flights arriving from NYC") +
    #geom_polygon(data= all_states) +
    theme_bw() + coord_map()


    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5))
  
  
  
  return(p)
}