library(data.table)

temporal.average <- function(dataseries) {
  
  # Check series cover the same timespan
  
  rbindlist(dataseries)[,lapply(.SD,mean), list(Lon, Lat)]
  
}