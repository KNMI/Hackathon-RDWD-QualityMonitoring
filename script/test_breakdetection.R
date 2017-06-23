# Script to analyse all AWS stations with MAN stations, and write the resulting break detections back in the database. 
# Basically this script is a loop around runScripts.R in order to calculate the BD output for 34 situations:
# namely the AWS mean with MAN mean, as well as each individual AWS timeserie with the averaged three connected MAN mean. 

setwd("~/Hackathon-RDWD-QualityMonitoring/")

Sys.setenv(R_CONFIG_ACTIVE = "test")
source("R/databaseOperations.R")
source("R/aggregateOperations.R")
source("R/averagingOperations.R")
source("R/timeseriesOperations.R")
source("R/breakDetection.R")

# Data input # 
# note: these functions will later be replaced by obtaining the data directly through the query.

obj <- db.execute(db.select.all, time.interval="1hour", type="H", element="RH")  # AWS
obj <- aggregate288(obj)
obj <- aggregate2Seasonal(obj)
obj <- aggregate2Year(obj) 
obj2 <- db.execute(db.select.all, time.interval="1day", type="N", element="RD")  # MAN
obj2 <- aggregate2Seasonal(obj2)
obj2 <- aggregate2Year(obj2) 

MAN_labels2 <- sapply(obj2$year$meta, function(m){m$sta_id})
MAN_labels <- c()
for(m in 1:length(MAN_labels2)){MAN_labels[m] <- MAN_labels2[[m]]}

obj3 <- db.execute(db.select.all, time.interval="1hour", type="N", element="RR") # Radar at MAN locations
obj3selec <- sapply(obj3$`1hour`$meta, function(m){m$sta_id %in% MAN_labels & m$sta_type=="n"})
obj3$`1hour`$data <- obj3$`1hour`$data[obj3selec]   #only take the radar timeseries at MAN stations
obj3$`1hour`$meta <- obj3$`1hour`$meta[obj3selec]   
obj3 <- aggregate288(obj3)
obj3 <- aggregate2Seasonal(obj3)
obj3 <- aggregate2Year(obj3) 

AWS_series <- names(obj$year$meta)
AWS_labels <- sapply(obj$year$meta, function(m){paste0(m$sta_id, "_H")})


for(n in 0:length(AWS_labels)){
  if(n == 0){
    Comb_Name <- "NL"     # all AWS vs all MAN vs all Radar
    obj_subset1 <- obj
    obj_subset2 <- obj2
    obj_subset3 <- obj3
  }else{
    Comb_Name <- AWS_labels[[n]]
    obj_subset1_y <- obj$year$data[n]  # list with 1 data.table. 
    obj_subset1_season <- obj$season$data[n]  # list with 1 data.table. 
    
    stations_nearby <- station.nearby(AWS_labels[[n]])$nearby_code_real
    if(length(stations_nearby)==0){next} # in case no nearby stations are available
    nearby_labels <- substr(stations_nearby, 1, (nchar(stations_nearby)-2))
    
    selec_obj2_y <- sapply(obj2$year$meta, function(m){m$sta_id %in% substr(stations_nearby, 1, nchar(stations_nearby)-2) }) 
    obj_subset2_y <- obj2$year$data[selec_obj2]
    selec_obj2_season <- sapply(obj2$season$meta, function(m){m$sta_id %in% substr(stations_nearby, 1, nchar(stations_nearby)-2) }) 
    obj_subset2_season <- obj2$season$data[selec_obj2]
    
    selec_obj3_y <- sapply(obj3$year$meta, function(m){m$sta_id %in% substr(stations_nearby, 1, nchar(stations_nearby)-2) }) 
    obj_subset3_y <- obj3$year$data[selec_obj3]
    selec_obj3_season <- sapply(obj3$year$meta, function(m){m$sta_id %in% substr(stations_nearby, 1, nchar(stations_nearby)-2) }) 
    obj_subset3_season <- obj3$year$data[selec_obj3]
  }
  
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
  
  
  # Break detection #
  
  BD_y <- break.detection(rel_dif_AWSvsMAN_y)
  BD_djf <- break.detection(rel_dif_AWSvsMAN_djf)
  BD_mam <- break.detection(rel_dif_AWSvsMAN_mam)
  BD_jja <- break.detection(rel_dif_AWSvsMAN_jja)
  BD_son <- break.detection(rel_dif_AWSvsMAN_son)
  
  
} # end n-loop


