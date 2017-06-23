#' @title Rain rate plotting
#' @description Visualisation function of rain rate timelines of the selection of stations and their average. 
#' @details When selecting subsets of stations that should be compared, the rain rate of each individual station as well as the overall averaged rain rate of both subsets are plotted in time. 
#' #@example RainRatePlotting(timeseries1, timeseries2, averagedtimeseries1, averagedtimeseries2, timespan1=as.Date("20000101", format="%Y%m%d"), timespan2=as.Date("20150101", format="%Y%m%d"))
#' @param timeseries1 a list of timeseries, in data.table format containing datetime and value columns, that are to be compared with a validation selection of stations. Each element in the list has the name of its seriesID. 
#' @param timeseries2 a list of timeseries, in data.table format containing datetime and value columns, that make up the validation selection of stations. Each element in the list has the name of its seriesID. 
#' @param averagedtimeseries1 the averaged timeline of the subset in timelines 1, as data.table containing datetime and value columns. Note that this is the output from the average.spatial function.
#' @param averagedtimeseries2 the averaged timeline of the subset in timelines 2, as data.table containing datetime and value columns. Note that this is the output from the average.spatial function.
#' @param timespan1 lower bound value in as.Date format that span up the range over which the timeseries should be plotted. If not provided, the entire period provided in the input data is plotted. 
#' @param timespan2 upper bound value in as.Date format that span up the range over which the timeseries should be plotted. If not provided, the entire period provided in the input data is plotted. 
#' @author Lotte

RainRatePlotting <- function(timeseries1, timeseries2, averagedtimeseries1, averagedtimeseries2, timespan1=NULL, timespan2=NULL) {
  
  #plot over entire period. 
  if(is.null(timespan1) & is.null(timespan2)) {
    
    timespan1 <- as.Date(min(sapply(c(timeseries1, timeseries2), function(ts) min(ts$datetime))), format="%Y%m%d%H%M%S", tz="GMT")
    timespan2 <- as.Date(max(sapply(c(timeseries1, timeseries2), function(ts) max(ts$datetime))), format="%Y%m%d%H%M%S", tz="GMT") + 1
  
    #let the timeseries start at the first row where the value is not NA
    for(m in 1:length(timeseries1)){
      if(length(which(is.na(timeseries1[[m]]$value)==F)) < 1){next}
      timeseries1[[m]] <- timeseries1[[m]][(which(is.na(timeseries1[[m]]$value)==F)[1]): nrow(timeseries1[[m]]),] }
    for(m in 1:length(timeseries2)){
      if(length(which(is.na(timeseries2[[m]]$value)==F)) < 1){next}
      timeseries2[[m]] <- timeseries2[[m]][(which(is.na(timeseries2[[m]]$value)==F)[1]): nrow(timeseries2[[m]]),] }
     
  #plot over specific interval. 
  } else {
    if((is.null(timespan1)==F & is.null(timespan2)) | (is.null(timespan2)==F & is.null(timespan1))){
      stop("Provide either both timespan1 and timespan 2, or neither.")}
      if(class(timespan1) != "Date" | class(timespan2) != "Date"){
        stop("timespan1 and timespan2 should be provided in class 'Date'.")}
      if(timespan2 <= timespan1){
        stop("timespan2 should indicate a later time than timespan1.")}
      
      
            #let the timeseries start at the first row where the value is not NA
      for(m in 1:length(timeseries1)){
        timeseries1[[m]] <- timeseries1[[m]][which(as.Date(timeseries1[[m]]$datetime, format="%Y%m%d%H%M%S", tz="GMT") >= timespan1 & as.Date(timeseries1[[m]]$datetime, format="%Y%m%d%H%M%S", tz="GMT") <= timespan2),]
        if(length(which(is.na(timeseries1[[m]]$value)==F)) < 1){next}
        timeseries1[[m]] <- timeseries1[[m]][(which(is.na(timeseries1[[m]]$value)==F)[1]): nrow(timeseries1[[m]]),] }
      for(m in 1:length(timeseries2)){
        timeseries2[[m]] <- timeseries2[[m]][which(as.Date(timeseries2[[m]]$datetime, format="%Y%m%d%H%M%S", tz="GMT") >= timespan1 & as.Date(timeseries2[[m]]$datetime, format="%Y%m%d%H%M%S", tz="GMT") <= timespan2),]
        if(length(which(is.na(timeseries2[[m]]$value)==F)) < 1){next}
        timeseries2[[m]] <- timeseries2[[m]][(which(is.na(timeseries2[[m]]$value)==F)[1]): nrow(timeseries2[[m]]),] }
      averagedtimeseries1 <- averagedtimeseries1[which(as.Date(averagedtimeseries1$datetime, format="%Y%m%d%H%M%S", tz="GMT") >= timespan1 & as.Date(averagedtimeseries1$datetime, format="%Y%m%d%H%M%S", tz="GMT") <= timespan2),]
      averagedtimeseries2 <- averagedtimeseries2[which(as.Date(averagedtimeseries2$datetime, format="%Y%m%d%H%M%S", tz="GMT") >= timespan1 & as.Date(averagedtimeseries2$datetime, format="%Y%m%d%H%M%S", tz="GMT") <= timespan2),]
      
      
  }    
    
  
    
    #Cumsum is the cumulative rainfall. Note that in case of NA values, 0 rainfall is added to the cumulative sum. 
    Cumsum1 <- cumsum(ifelse(is.na( averagedtimeseries1$value ), 0, averagedtimeseries1$value )) / 10
    startvalue <- sapply(timeseries1, function(ts) c(0,Cumsum1)[which(averagedtimeseries1==ts$datetime[1])])
    
    plot(as.Date(averagedtimeseries1$datetime, format="%Y%m%d%H%M%S", tz="GMT"), Cumsum1, xlim=c(timespan1, timespan2), xlab="Time", ylab="Cumulative rainfall (mm)", col="red", type="l")
    for(n in 1:length(timeseries1)){
      if(nrow(timeseries1[[n]])==0){next}
      lines(as.Date(timeseries1[[n]]$datetime, format="%Y%m%d%H%M%S", tz="GMT"), (cumsum(ifelse(is.na( timeseries1[[n]]$value ), 0, timeseries1[[n]]$value )) + timeseries1[[n]]$value *0) / 10 + startvalue[[n]], col=rgb(1, 0, 0, 0.1))}

    #Plot lines for timeseries2
    Cumsum2 <- cumsum(ifelse(is.na( averagedtimeseries2$value ), 0, averagedtimeseries2$value )) / 10
    startvalue <- sapply(timeseries2, function(ts) c(0,Cumsum2)[which(averagedtimeseries2==ts$datetime[1])])
    
    lines(as.Date(averagedtimeseries2$datetime, format="%Y%m%d%H%M%S", tz="GMT"), Cumsum2, col="blue")
    for(n in 1:length(timeseries2)){
      if(nrow(timeseries2[[n]])==0){next}
      lines(as.Date(timeseries2[[n]]$datetime, format="%Y%m%d%H%M%S", tz="GMT"), (cumsum(ifelse(is.na( timeseries2[[n]]$value ), 0, timeseries2[[n]]$value )) + timeseries2[[n]]$value *0) / 10 + startvalue[[n]], col=rgb(0, 0, 1, 0.1))}
    
  legend("topleft", c("subset 1", "subset 2"), col=c("red", "blue"), lty=1)
}
    