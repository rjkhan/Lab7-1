visualize airport delays <- function(){
  a <- select(flights, one_of(c("dep_delay", "arr_delay", "origin", "dest")))
  b <- select(airports, one_of(c("faa", "lat", "lon")))
  k <- filter(b,faa %in% c(unique(a$dest)))
  jfk <- filter(a, origin == "JFK" & !is.na(dep_delay))
  ewr <- filter(a, origin == "EWR" & !is.na(dep_delay))
  lga <- filter(a, origin == "LGA" & !is.na(dep_delay))
  dep_jfk <- summarise(jfk, dep_jfk = mean(dep_delay))
  dep_ewr <- summarise(ewr, dep_ewr = mean(dep_delay))
  dep_lga <- summarise(lga, dep_lga = mean(dep_delay))
  
  arrivaldelays <- flights %>% filter(!is.na(arr_delay)) %>% group_by(dest) %>% 
    summarize(avg=mean(arr_delay))
  # it seems like in the data with latitude and longitude there aren't some airports that can be found in arrivaldelays
  k1 <- rename(k, dest = faa)
   papa <- inner_join(k1, arrivaldelays, by = "dest")
  
  
  
  
}