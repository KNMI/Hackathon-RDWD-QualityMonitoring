library(data.table)

temporal.average <- function(dataseries) {
  
  combined <- Reduce(function(X,Y) X[Y], dataseries)

  rowMeans(combined[,2:ncol(combined)])
  

}