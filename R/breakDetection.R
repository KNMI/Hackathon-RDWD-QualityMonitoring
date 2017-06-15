#' @title Break detection with Buishand method
#' @description Function for break detection in a yearly or seasonal series that uses the Buishand cumulative test. It needs a table with two columns (datetime and value). the input is temporarily downloaded by the dummy data
#' @author Antonello Squintu and Hidde

break.detection <- function(series1, range = 2, plot.score = FALSE) {
  #series1 is a data.table with datetime and value elements
  #Package "climtrends" is needed
  library("climtrends")
  
  #Check if dataset is long enough (if not, return NULL)
  if (length(series1$value) < as.numeric(row.names(Buishand.Critical.Values[1,]))) {
    return(integer(0))
  }
  
  #Buishand test
  bd_buishand <- data.frame(year = series1$datetime)
  bd_buishand$score <- round(BuishandRangeTest(series1$value), digits = 2)
  
  cv <- Buishand.Critical.Values[max(which(length(series1$value) >= as.numeric(row.names(Buishand.Critical.Values)))),2]
  
  cy <- bd_buishand[abs(bd_buishand$score) > cv, ]
  bd_buishand$TF <- FALSE
  
  #Research of local maxima in the critical years
  if (nrow(cy) > 0) {
    for(i in 1 : nrow(cy))
    {
      if (cy$year[i] > bd_buishand$year[1] + range && cy$year[i] < bd_buishand$year[nrow(bd_buishand)] - range)
      {
        aux <- bd_buishand[bd_buishand$year %in% (cy$year[i] - range) : (cy$year[i] + range),]
        if (which.max(abs(aux$score)) == range + 1)
        {
          bd_buishand[bd_buishand$year == cy$year[i], 'TF'] = TRUE
        }
      }
    }
  }
  
  if (plot.score) {
    plot(bd_buishand$year,bd_buishand$score,type='l',
         main='Buishand Test',
         xlab='year',
         ylab='buishand score',
         ylim=c(min(-cv,bd_buishand$score),max(cv,bd_buishand$score)))
    points(bd_buishand$year,bd_buishand$score,pch=16)
    abline(h=cv,col='darkgreen',lwd=2)
    abline(h=-cv,col='darkgreen',lwd=2)
    abline(v=bd_buishand$year[bd_buishand$TF],col='red',lwd=2)
  }
  
  return(bd_buishand$year[bd_buishand$TF])
}
