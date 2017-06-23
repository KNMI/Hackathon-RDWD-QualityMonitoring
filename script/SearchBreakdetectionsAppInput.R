# script to search for breakdetections based on the subset selections obtained via the app user interface. 
# the script should be one big funciton that makes use of the app output and automatically generates the years with breakdetections, 
# as well as a plot of relative differences with MAN stations and with Radar values at those stations. 

#' Calculate seasonal sums
#' @title Break detection of app input
#' @description Function plots relative differences of station with selected subset MAN and Radar values, where breaks are clearly identified. 
#' @details The function generates timelines of relative precipitation differences, as well as breakpoints (as compared to stations measurements), and visualizes this in 5 plots: full year and seasonal. The plot automatically visualises the entire period that is available. 
#' @param station first row of output from App. This row should contain information from which type and element can be derived. 
#' @param subsetselection table with rows of output from App. Each row should contain information from which type and element can be derived.
#' @example BreakDetectionFinder(stationID = "260_H", subsetselectionIDs = c("10_N",  "11_N",  "12_N",  "13_N", "220_H"))
#' @author Lotte
#' @export 5 plots of relative differences between the selected station and the average of the selected subset station values as well as the radar at those locations, with red indicators if a break is found. 

# keep this in working memory if query doesn't work. Warning: takes a long!
obj <- db.execute(db.select.all, time.interval="1hour", type="H", element="RH")  # AWS
obj <- aggregate.to.88.2(obj)
obj <- aggregate.to.seasonal.2(obj)
obj <- aggregateTo.year(obj) 
obj2 <- db.execute(db.select.all, time.interval="1day", type="N", element="RD")  # MAN
obj2 <- aggregate.to.seasonal.2(obj2)
obj2 <- aggregateTo.year(obj2) 
obj3a <- db.execute(db.select.all, time.interval="1hour", type="H", element="RR") # Radar at all locations
obj3b <- db.execute(db.select.all, time.interval="1hour", type="N", element="RR") # Radar at all locations
obj3a <- aggregate.to.88.2(obj3a)
obj3a <- aggregate.to.seasonal.2(obj3a)
obj3a <- aggregateTo.year(obj3a) 
obj3b <- aggregate.to.88.2(obj3b)
obj3b <- aggregate.to.seasonal.2(obj3b)
obj3b <- aggregateTo.year(obj3b) 


BreakDetectionFinder <- function(stationID, subsetselectionIDs){

Sys.setenv(R_CONFIG_ACTIVE = "test")
source("R/databaseOperations.R")
source("R/aggregateOperations.R")
source("R/averagingOperations.R")
source("R/timeseriesOperations.R")
source("R/breakDetection.R")
cfg <- config::get(file = "config/config.yml")

# Data input # 
station.ID <- substr(stationID, 1, (nchar(stationID)-2))
type.station <- substr(stationID, nchar(stationID), nchar(stationID))
if(type.station=="H"){
  element.station <- "RH"}
if(type.station=="N"){
  element.station <- "RD"}


subset.IDs <- substr(subsetselectionIDs, 1, (nchar(subsetselectionIDs)-2))
type.subset <- substr(subsetselectionIDs, nchar(subsetselectionIDs), nchar(subsetselectionIDs))  
element.subset <- rep(NA, length(type.subset))
 element.subset[which(type.subset == "H")] <- "RH" 
 element.subset[which(type.subset == "N")] <- "RD"   

########### 
 # find relevant values in obj. This is a temporary solution until we can make a query to obtain these values directly from the database. 
#station selection 
 if(type.station == "H"){
 obj1selec <- sapply(obj$year$meta, function(m){m$sta_id == station.ID})
 obj_subset1_y <- obj$year$data[obj1selec]  # list with 1 data.table. 
 obj_subset1_season <- obj$season$data[obj1selec]}  # list with 1 data.table. 
 if(type.station =="N"){
 obj1selec <- sapply(obj2$year$meta, function(m){m$sta_id == station.ID})
 obj_subset1_y <- obj2$year$data[obj1selec]  # list with 1 data.table. 
 obj_subset1_season <- obj2$season$data[obj1selec]}  # list with 1 data.table. 

#subset selection from subset AWS and MAN
 obj2subsetselec1 <- sapply(obj$year$meta, function(m){m$sta_id %in% subset.IDs})
 obj2subsetselec2 <- sapply(obj2$year$meta, function(m){m$sta_id %in% subset.IDs})
 obj_subset2_y <- c(obj$year$data[obj2subsetselec1] , obj2$year$data[obj2subsetselec2]) 
 obj_subset2_season <- c(obj$season$data[obj2subsetselec1] , obj2$season$data[obj2subsetselec2]) 
 
#subset selection from obj3a and obj3b for radar
 obj3subsetselec1 <- sapply(obj3a$year$meta, function(m){m$sta_id %in% subset.IDs})
 obj3subsetselec2 <- sapply(obj3b$year$meta, function(m){m$sta_id %in% subset.IDs})
 obj_subset3_y <- c(obj3a$year$data[obj3subsetselec1] , obj3b$year$data[obj3subsetselec2])
 obj_subset3_season <- c(obj3a$season$data[obj3subsetselec1] , obj3b$season$data[obj3subsetselec2])

##############  

  # Spatial averaging #
  
  obj1_average_y  <- average.spatial(timeseries=obj_subset1_y) 
  obj2_average_y  <- average.spatial(timeseries=obj_subset2_y) 
  obj3_average_y  <- average.spatial(timeseries=obj_subset3_y) 
  
  obj1_average_season  <- average.spatial(timeseries=obj_subset1_season) 
  obj2_average_season  <- average.spatial(timeseries=obj_subset2_season) 
  obj3_average_season  <- average.spatial(timeseries=obj_subset3_season) 
  
  obj1_average_djf <- obj1_average_season[which(month(as.Date(obj1_average_season$datetime, format="%Y%m%d%H%M%S")) == 3)]
  obj1_average_mam <- obj1_average_season[which(month(as.Date(obj1_average_season$datetime, format="%Y%m%d%H%M%S")) == 6)]
  obj1_average_jja <- obj1_average_season[which(month(as.Date(obj1_average_season$datetime, format="%Y%m%d%H%M%S")) == 9)]
  obj1_average_son <- obj1_average_season[which(month(as.Date(obj1_average_season$datetime, format="%Y%m%d%H%M%S")) == 12)]
  
  obj2_average_djf <- obj2_average_season[which(month(as.Date(obj2_average_season$datetime, format="%Y%m%d%H%M%S")) == 3)]
  obj2_average_mam <- obj2_average_season[which(month(as.Date(obj2_average_season$datetime, format="%Y%m%d%H%M%S")) == 6)]
  obj2_average_jja <- obj2_average_season[which(month(as.Date(obj2_average_season$datetime, format="%Y%m%d%H%M%S")) == 9)]
  obj2_average_son <- obj2_average_season[which(month(as.Date(obj2_average_season$datetime, format="%Y%m%d%H%M%S")) == 12)]
  
  obj3_average_djf <- obj3_average_season[which(month(as.Date(obj3_average_season$datetime, format="%Y%m%d%H%M%S")) == 3)]
  obj3_average_mam <- obj3_average_season[which(month(as.Date(obj3_average_season$datetime, format="%Y%m%d%H%M%S")) == 6)]
  obj3_average_jja <- obj3_average_season[which(month(as.Date(obj3_average_season$datetime, format="%Y%m%d%H%M%S")) == 9)]
  obj3_average_son <- obj3_average_season[which(month(as.Date(obj3_average_season$datetime, format="%Y%m%d%H%M%S")) == 12)]
  
  
  # Calculate relative difference # 
  
  rel_dif_AWSvsMAN_y <- timeseries.relative.difference(timeserie1=obj1_average_y, timeserie2=obj2_average_y)
  rel_dif_AWSvsRAD_y <- timeseries.relative.difference(timeserie1=obj1_average_y, timeserie2=obj3_average_y)
  
  rel_dif_AWSvsMAN_djf <- timeseries.relative.difference(timeserie1=obj1_average_djf, timeserie2=obj2_average_djf)
  rel_dif_AWSvsRAD_djf <- timeseries.relative.difference(timeserie1=obj1_average_djf, timeserie2=obj3_average_djf)
  rel_dif_AWSvsMAN_mam <- timeseries.relative.difference(timeserie1=obj1_average_mam, timeserie2=obj2_average_mam)
  rel_dif_AWSvsRAD_mam <- timeseries.relative.difference(timeserie1=obj1_average_mam, timeserie2=obj3_average_mam)
  rel_dif_AWSvsMAN_jja <- timeseries.relative.difference(timeserie1=obj1_average_jja, timeserie2=obj2_average_jja)
  rel_dif_AWSvsRAD_jja <- timeseries.relative.difference(timeserie1=obj1_average_jja, timeserie2=obj3_average_jja)
  rel_dif_AWSvsMAN_son <- timeseries.relative.difference(timeserie1=obj1_average_son, timeserie2=obj2_average_son)
  rel_dif_AWSvsRAD_son <- timeseries.relative.difference(timeserie1=obj1_average_son, timeserie2=obj3_average_son)
  
  
  BDyear <- break.detection(rel_dif_AWSvsMAN_y,name=Comb_Name,type='y',plot.score = FALSE)
  BDdjf <- break.detection(rel_dif_AWSvsMAN_djf,name=Comb_Name,type='djf',plot.score = FALSE)
  BDmam <- break.detection(rel_dif_AWSvsMAN_mam,name=Comb_Name,type='mam',plot.score = FALSE)
  BDjja <- break.detection(rel_dif_AWSvsMAN_jja,name=Comb_Name,type='jja',plot.score = FALSE)
  BDson <- break.detection(rel_dif_AWSvsMAN_son,name=Comb_Name,type='son',plot.score = FALSE)
  
  
  dev.new() #year
  if(length(BDyear) == 0){ main <- paste0("Year: no breaks detected")
  }else{ main <- paste0("Year: breaks detected")}
  plot(as.numeric(substr(rel_dif_AWSvsMAN_y$datetime, 1,4)), rel_dif_AWSvsMAN_y$value, type="l", xlab="Time", ylab="Relative difference [%]", main=main, 
       ylim=range(c(rel_dif_AWSvsMAN_y$value,rel_dif_AWSvsRAD_y$value), na.rm=T), xlim=c(1981, 2017))
  lines(as.numeric(substr(rel_dif_AWSvsRAD_y$datetime, 1,4)), rel_dif_AWSvsRAD_y$value, col="purple")
  abline(v=BDyear, col="red")
  abline(h=0, col="grey")
  legend("bottomleft", c("station vs other stations", "station vs radar", "break"), col=c("black", "purple", "red"), lty=1)
  
  dev.new() #djf
  if(length(BDdjf) == 0){ main <- paste0("Winter: no breaks detected")
  }else{ main <- paste0("Winter: breaks detected")}
  plot(as.numeric(substr(rel_dif_AWSvsMAN_djf$datetime, 1,4)), rel_dif_AWSvsMAN_djf$value, type="l", xlab="Time", ylab="Relative difference [%]", main=main, 
       ylim=range(c(rel_dif_AWSvsMAN_djf$value,rel_dif_AWSvsRAD_djf$value), na.rm=T), xlim=c(1981, 2017))
  lines(as.numeric(substr(rel_dif_AWSvsRAD_djf$datetime, 1,4)), rel_dif_AWSvsRAD_djf$value, col="purple")
  abline(v=BDdjf, col="red")
  abline(h=0, col="grey")
  legend("bottomleft", c("station vs other stations", "station vs radar", "break"), col=c("black", "purple", "red"), lty=1)
  
  dev.new() #mam
  if(length(BDmam) == 0){ main <- paste0("Spring: no breaks detected")
  }else{ main <- paste0("Spring: breaks detected")}
  plot(as.numeric(substr(rel_dif_AWSvsMAN_mam$datetime, 1,4)), rel_dif_AWSvsMAN_mam$value, type="l", xlab="Time", ylab="Relative difference [%]", main=main, 
       ylim=range(c(rel_dif_AWSvsMAN_mam$value,rel_dif_AWSvsRAD_mam$value), na.rm=T), xlim=c(1981, 2017))
  lines(as.numeric(substr(rel_dif_AWSvsRAD_mam$datetime, 1,4)), rel_dif_AWSvsRAD_mam$value, col="purple")
  abline(v=BDmam, col="red")
  abline(h=0, col="grey")
  legend("bottomleft", c("station vs other stations", "station vs radar", "break"), col=c("black", "purple", "red"), lty=1)
  
  dev.new() #jja
  if(length(BDjja) == 0){ main <- paste0("Summer: no breaks detected")
  }else{ main <- paste0("Summer: breaks detected")}
  plot(as.numeric(substr(rel_dif_AWSvsMAN_jja$datetime, 1,4)), rel_dif_AWSvsMAN_jja$value, type="l", xlab="Time", ylab="Relative difference [%]", main=main, 
       ylim=range(c(rel_dif_AWSvsMAN_jja$value,rel_dif_AWSvsRAD_jja$value), na.rm=T), xlim=c(1981, 2017))
  lines(as.numeric(substr(rel_dif_AWSvsRAD_jja$datetime, 1,4)), rel_dif_AWSvsRAD_jja$value, col="purple")
  abline(v=BDjja, col="red")
  abline(h=0, col="grey")
  legend("bottomleft", c("station vs other stations", "station vs radar", "break"), col=c("black", "purple", "red"), lty=1)
  
  dev.new() #son
  if(length(BDson) == 0){ main <- paste0("Autumn: no breaks detected")
  }else{ main <- paste0("Autumn: breaks detected")}
  plot(as.numeric(substr(rel_dif_AWSvsMAN_son$datetime, 1,4)), rel_dif_AWSvsMAN_son$value, type="l", xlab="Time", ylab="Relative difference [%]", main=main, 
       ylim=range(c(rel_dif_AWSvsMAN_son$value,rel_dif_AWSvsRAD_son$value), na.rm=T), xlim=c(1981, 2017))
  lines(as.numeric(substr(rel_dif_AWSvsRAD_son$datetime, 1,4)), rel_dif_AWSvsRAD_son$value, col="purple")
  abline(v=BDson, col="red")
  abline(h=0, col="grey")
  legend("bottomleft", c("station vs other stations", "station vs radar", "break"), col=c("black", "purple", "red"), lty=1)
    
  } #end function
  





