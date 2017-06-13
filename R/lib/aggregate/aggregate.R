library(data.table)

#' Load in test data from Mariekes directory
grab.test.data <- function() {
  marieke.dir <- "/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata/"
  
  data.names <- lapply(list.files(marieke.dir), function(x) {
    name <- substr(x, 0, nchar(x) - 4)
  })
  data.tables <- lapply(list.files(marieke.dir), function(x) {
    fread(paste0(marieke.dir, x))  
  })
  names(data.tables) <- data.names
  
  return(data.tables)
}

#'
#' @param aggregate88 The 8-8 aggregated data to be further aggregated to seasonal
aggregate.to.seasonal <- function(aggregate88) {
  winter <- c(12, 1, 2)
  spring <- c(3, 4, 5)
  summer <- c(6, 7, 8)
  autumn <- c(9, 10, 11)
  
  names(aggregate88) <- c("date", "value")
  aggregate88$date <- as.Date(as.character(aggregate88$date), format="%Y%m%d")
  months <- month(aggregate88$date)
  
  aggregate88$season <- 0
  aggregate88$season[months %in% winter] <- "winter"
  aggregate88$season[months %in% spring] <- "spring"
  aggregate88$season[months %in% summer] <- "summer"
  aggregate88$season[months %in% autumn] <- "autumn"
  aggregate88$season <- factor(aggregate88$season)
  
  yearly.sums <- data.table(aggregate(list(y = aggregate88$value), by = list(year = year(aggregate88$date)), sum))
  
  aggregate.sums <- by(aggregate88, aggregate88$season, function(x) {
    data.table(aggregate(list(value = x$value), list(year = year(x$date)), sum))
  })
  
  aggregate.seasonal <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "year", all = TRUE), aggregate.sums)
  names(aggregate.seasonal) <- c("year", "djf", "mam", "jja", "son")
  aggregate.seasonal <- merge(yearly.sums, aggregate.seasonal, by = "year")
  
  return(aggregate.seasonal)
}

#'
#' @param raw The raw data to be aggregated to 8-8 data
#example:
#raw <- data.tables$`DeBilt_260_H_hourly_precip`

aggregate.to.88 <- function(raw) {
  
  names(raw) <- c("date", "hour", "value")
  idx <- which(sapply(raw$hour, nchar)==5)
  raw$hour[idx] <- paste0(0,raw$hour[idx])

  time <- substr(paste0(raw$date, raw$hour), 1,12)
  rain <- raw$value    

    # make timeline of timestamps 0800 indicating the end of each day
    first_timestep <- which(substr(time, 9,12) == "0900")[1]
    last_timestep <- which(substr(time, 9,12) == "0800")[length(which(substr(time, 9,12) == "0800"))]

    # aggregate rainfall in the 24 hours belonging to the 0800-0800 timeframe          
    nrdays <- length(first_timestep:last_timestep)/24
    time_agg <- rep(1:nrdays, each=24)
    rainselec <- rain[first_timestep:last_timestep]  
    rain_agg <- setDT(as.data.frame(rainselec))[,lapply(.SD,sum),by=.(time_agg)]$rainselec
    
    # output file
    aggregated_data <- raw[seq((first_timestep+23), last_timestep, by=24),]
    aggregated_data$value <- rain_agg

    return(aggregated_data)

}







