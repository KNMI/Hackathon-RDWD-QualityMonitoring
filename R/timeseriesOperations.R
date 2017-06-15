library(data.table)

#' @title Relative difference of timeseries
#' @description Calculate the relative difference between two timeseries as 100% * (timeseries2 - timeseries1)/timeseries1.
#' @details the input should have certain column names and identical timeseries. If not, the function stops with an error that identifies the problem. 
#' @example input; files <- grab.test.data(); AWS_timeserie <- files$Almelo_664_N_seasonal_precip[,c(1,3)]; Man_timeserie <- files$Enschede_665_N_seasonal_precip[,c(1,3)]; names(AWS_timeserie) <- names(Man_timeserie) <- c("time", "average")
#' @param timeserie1 timeserie as data.table with first column datetime and second column (average) value. Possible input is AWS_timeserie. 
#' @param timeserie2 timeserie as data.table with first column datetime and second column (average) value. Possible input is MAN_timeserie. 
#' @author Lotte and Jurian
timeseries.relative.difference <- function(timeserie1, timeserie2){
  
  # control statements
  if(names(timeserie1)[1] != "datetime" | names(timeserie1)[2] != "value" | names(timeserie2)[1] != "datetime" | names(timeserie2)[2] != "value")
    stop("Names of input file columns are not 'time' and/or 'average'")
  
  if(length(which(timeserie1$time != timeserie2$time) > 0)) 
    stop("Timeperiods of input files do not match")
  
  relative_dif <- 100 * (timeserie2$value - timeserie1$value) / timeserie1$value
  relative_dif[timeserie1$value == 0 & timeserie2$value == 0] <- 0
  relative_dif[timeserie1$value == 0 & timeserie2$value != 0] <- NA
  
  output <- data.table(datetime=timeserie2$datetime, value=relative_dif)
  
  return(output)
}

#' @title Intersect multiple timeseries on their common datetime value
#' @details NB. This function assumes that all timeseries are complete!
#' @description Supposed to be very fast and efficient, i.e. very fast even on millions of observations
#' @param timeseries A list of data.tables, each a timeseries of structure <datetime, value> 
#' @return A list of data.tables, each a timeseries of structure <datetime, value>. Only with observations that all timeseries have in common
#' @author Jurian and Hidde
timeseries.intersection <- function(timeseries) {
  
  # Find the latest start datetime and earliest stop datetime
  datetime.start <- max(sapply(timeseries, function(ts) min(ts$datetime)))
  datetime.stop <- min(sapply(timeseries, function(ts) max(ts$datetime)))

  # We make use of data.tables fast fintersect() function
  # The datetime column is already a key, which makes this very fast
  # Not used ATM, keeping it here because code might be useful elsewhere
  #datetime.intersection <- unlist(Reduce(function(X,Y) fintersect(X[,"datetime"],Y[,"datetime"]), timeseries))
  
  lapply(timeseries, function(ts) {
    subset(ts, datetime >= datetime.start & datetime <= datetime.stop)
  })
}
