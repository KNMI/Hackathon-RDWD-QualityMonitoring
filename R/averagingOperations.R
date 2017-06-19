library(data.table)
library(config)

#' @title Create an average over several stations
#' @param timeseries A list of data.tables, each a timeseries of structure <datetime, value>
#' @return A single timeserie of structure <datetime, value>
#' @author Hidde and Jurian
#' @export
average.spatial <- function(timeseries) {
  
  cfg <- config::get(file = "config/config.yml")
  nstations <- length(timeseries)
  
  # Give station values unique names so we can merge them
  for(i in 1:nstations) {
    names(timeseries[[i]])[2] <- paste0("value", ".", i)
  }
  # Merge all stations
  combined <- Reduce(function(X, Y) base::merge(X, Y, by = "datetime", all = T), timeseries)
  
  dt <- data.table(datetime = combined$datetime, value = rowMeans(combined[,-1], na.rm = T))
  dt$value[1 - rowSums(is.na(combined)) / nstations < cfg$data.availability.threshold.averaging] <- NA
  
  t.order = order(dt$datetime)
  dt = dt[t.order, ]
  
  return(dt)
}