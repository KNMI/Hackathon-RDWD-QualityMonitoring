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
  
  # Define meteorological seasons
  winter <- c(12, 1, 2)
  spring <- c(3, 4, 5)
  summer <- c(6, 7, 8)
  autumn <- c(9, 10, 11)
  
  names(aggregate88) <- c("date", "value")
  # Convert the character vector to a date so we can perform operations like month() and year()
  aggregate88$date <- as.Date(as.character(aggregate88$date), format="%Y%m%d")
  
  # Collect months
  months <- month(aggregate88$date)
  
  # Assign seasons to observations
  aggregate88$season <- 0
  aggregate88$season[months %in% winter] <- "winter"
  aggregate88$season[months %in% spring] <- "spring"
  aggregate88$season[months %in% summer] <- "summer"
  aggregate88$season[months %in% autumn] <- "autumn"
  aggregate88$season <- factor(aggregate88$season)
  
  # Create a data.table for calendar yearly sums
  yearly.sums <- data.table(aggregate(list(y = aggregate88$value), by = list(year = year(aggregate88$date)), sum))
  
  # Create a list of data.tables, one for each season, this makes it easier to merge later on
  aggregate.sums <- by(aggregate88, aggregate88$season, function(x) {
    seasonal.sum <- data.table(aggregate(list(value = x$value), list(year = year(x$date)), sum))
    # Set key for really fast merging later on
    setkey(seasonal.sum, year)
    return(seasonal.sum)
  })
  
  # Merge all data tables in the list
  aggregate.seasonal <- Reduce(function(a, b) merge(a, b, by = "year", all = TRUE), aggregate.sums)
  # Fix names so we can merge the yearly sums
  names(aggregate.seasonal) <- c("year", "djf", "mam", "jja", "son")
  # Merge the yearly sums with the seasonal sums
  aggregate.seasonal <- merge(yearly.sums, aggregate.seasonal, by = "year")
  
  # Profit!
  return(aggregate.seasonal)
}

#'
#' @param hourly The hourly data to be aggregated to 8-8 data
#' @example hourly <- aggregate.to.88(data.tables$`DeBilt_260_H_hourly_precip`)
aggregate.to.88 <- function(hourly) {
  
  names(hourly) <- c("date", "hour", "value")
  idx <- which(nchar(hourly$hour) == 5)
  hourly$hour[idx] <- paste0(0,hourly$hour[idx])

  time <- substr(paste0(hourly$date, hourly$hour), 1,12)
  rain <- hourly$value    

    # make timeline of timestamps 0800 indicating the end of each day
    first_timestep <- which(substr(time, 9,12) == "0900")[1]
    last_timestep <- which(substr(time, 9,12) == "0800")[length(which(substr(time, 9,12) == "0800"))]

    # aggregate rainfall in the 24 hours belonging to the 0800-0800 timeframe          
    nrdays <- length(first_timestep:last_timestep)/24
    time_agg <- rep(1:nrdays, each=24)
    rainselec <- rain[first_timestep:last_timestep]  
    rain_agg <- setDT(as.data.frame(rainselec))[,lapply(.SD,sum),by=.(time_agg)]$rainselec
    
    # output file
    aggregated_data <- hourly[seq((first_timestep+23), last_timestep, by=24),]
    aggregated_data$value <- rain_agg

    return(aggregated_data)

}







