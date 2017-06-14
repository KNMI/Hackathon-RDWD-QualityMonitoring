#' @title Relative difference of timeseries
#' @description Calculate the relative difference between AWS timeserie and Man timeserie.
#' @details the input should have certain column names and identical timeseries. If not, the function stops with an error that identifies the problem. 
#' @example input; files <- grab.test.data(); AWS_timeserie <- files$Almelo_664_N_seasonal_precip[,c(1,3)]; Man_timeserie <- files$Enschede_665_N_seasonal_precip[,c(1,3)]; names(AWS_timeserie) <- names(Man_timeserie) <- c("time", "average")
#' @param AWS_timeserie The input contains 1 timecolumn and only 1 value column with AWS precipitation averages named 'average', and NA values are indicated with '-9999'.
#' @param Man_timeserie The input contains 1 timecolumn and only 1 value column with Man precipitation averages named 'average', and NA values are indicated with '-9999'.
#' @author Antonello
relative.difference <- function(AWS_timeserie, Man_timeserie){
  
  # control statements
  if(names(AWS_timeserie)[1] != "time" | names(AWS_timeserie)[2] != "average" | names(Man_timeserie)[1] != "time" | names(Man_timeserie)[2] != "average")
    stop("Names of input file columns are not 'time' and/or 'average'")
  
  if(length(which(AWS_timeserie$time != Man_timeserie$time) > 0)) stop("Timeperiods of input files do not match")
  
  AWS_timeserie$average[which(AWS_timeserie$average == -9999)] <- NA
  Man_timeserie$average[which(Man_timeserie$average == -9999)] <- NA
  
  relative_dif <- 100 * (Man_timeserie$average - AWS_timeserie$average) / AWS_timeserie$average
  # relative_dif[which(is.na(relative_dif))] <- -9999   # in case we save in the meantime turn on. 
  output <- data.table(time=Man_timeserie$time, relative_dif=relative_dif)
  
  return(output)
}

