library(data.table)
library(config)

#' @title Create an average over several stations
#' @param timeseries A list of data.tables, each a timeseries of structure <datetime, value>
#' @return A single timeserie of structure <datetime, value>
#' @author Hidde and Jurian
average.spatial <- function(timeseries) {
  
  cfg <- config::get(file = "config/config.yml")
  nstations <- length(timeseries)
  
  combined <- Reduce(function(X,Y) X[Y], timeseries)
  
  dt <- data.table(datetime = combined$datetime, value = rowMeans(combined[,-1], na.rm = T))
  dt$value[1 - rowSums(is.na(combined)) / nstations < cfg$data.availability.threshold] <- NA

  return(dt)
}