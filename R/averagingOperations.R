# library(data.table)
# library(config)

#' @title Create an average over several stations
#' @param timeseries A list of data.tables, each a timeseries of structure <datetime, value>
#' @return A single timeserie of structure <datetime, value>
#' @author Hidde and Jurian
#' @export
average.spatial <- function(timeseries) {
  
  cfg <- config::get(file = "config/config.yml")
  nstations <- length(timeseries)
  
  #combined <- Reduce(function(X,Y) X[Y], timeseries)
  
  combined <- timeseries[[1]]
  names(combined)[2] <- "col.1" 
  for(i in 2:(length(timeseries))){
   combined <- base::merge(combined, timeseries[[i]], by="datetime", all=T)
   names(combined)[i+1] <- paste0("col.",i)
  }
  
  dt <- data.table(datetime = combined$datetime, value = rowMeans(combined[,-1], na.rm = T))
  dt$value[1 - rowSums(is.na(combined)) / nstations < cfg$data.availability.threshold.averaging] <- NA
  
  t.order = order(dt$datetime)
  dt = dt[t.order, ]
  
  return(dt)
}