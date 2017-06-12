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
aggregate.to.88 <- function(raw) {
  
}







