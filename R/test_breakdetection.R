#' @title Break detection with Buishand method
#' @description Function for break detection in a yearly or seasonal series that uses the Buishand cumulative test. It needs a table with two columns (datetime and value). the input is temporarily downloaded by the dummy data
#' @param series1 time series to check, it must be a table with two columns (datetime and value)
#' @param range radius (in years) of the running window that is used for the identification of local minima/maxima
#' @param plot.score if TRUE it produces a plot for the visualization of the breaks
#' @author Antonello and Hidde

break.detection <- function(series1, range = 2, plot.score = FALSE) {
  #series1 is a data.table with datetime and value elements
  #range indicates the radius (in years) of the temporal interval that is ysed for the identification of local minima/maxima (see below for more details)
  #Package "climtrends" is needed
  
  
  
  
  #Check if dataset is long enough (if not, return NULL)
  if (length(series1$value) < as.numeric(row.names(Buishand.Critical.Values[1,]))) {
    return(integer(0))
  }
  
  ### Extract year from datetime
  
  series1$year <- year(as.Date(series1$datetime,format= "%Y%m%d%H%M%S")-1)
  
  ###Buishand test
  #Define data frame that will store the results of Buishand test 
  bd_buishand <- data.frame(year = series1$year)
  #Apply Buishand test to the series and store the result in the column "store" 
  bd_buishand$score <- round(BuishandRangeTest(series1$value), digits = 2)
  
  #Determine the Critical Value (it depends on the length of the series and on the required significance level, 95% in this case)
  cv <- Buishand.Critical.Values[max(which(length(series1$value) >= as.numeric(row.names(Buishand.Critical.Values)))),2]
  
  #Identify those years (Critical Years) whose score (without sign) is larger than the critical value
  cy <- bd_buishand[abs(bd_buishand$score) > cv, ]
  
  #Define a column where TRUE value indicates the location of a break and FALSE indicates other years
  bd_buishand$TF <- FALSE
  
  #Choosing all the years having score larger than the cv is not enough to identify the breaks.
  #Research of local minima/maxima in the critical years. In case of consecutive min/max or having two years of difference, the first one is labeled as break
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
