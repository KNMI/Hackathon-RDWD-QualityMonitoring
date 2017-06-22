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

obj <- db.execute(db.select.all, time.interval="1hour", type="H", element="RH")  # AWS
obj <- aggregate.to.88.2(obj)
obj2 <- db.execute(db.select.all, time.interval="1day", type="N", element="RD")  # MAN
obj3 <- db.execute(db.select.all, time.interval="1hour", type="N", element="RR") # Radar at MAN locations
obj3 <- aggregate.to.88.2(obj3)

AWS_series <- names(obj$`1hour`$meta)
AWS_labels <- sapply(obj$`1hour`$meta, function(m){paste0(m$sta_id, "_H")})


for(n in 0:length(AWS_labels)){
  if(n == 0){
    Comb_Name <- "NL"     # all AWS vs all MAN vs all Radar
    obj_subset1 <- obj
    obj_subset2 <- obj2
    obj_subset3 <- obj3
  }else{
    Comb_Name <- AWS_labels[[n]]
    obj_subset1 <- obj$`1day`$data[n]  # list with 1 data.table. 
    stations_nearby <- station.nearby(AWS_labels[[n]])$nearby_code_real
    if(length(stations_nearby)==0){next} # in case no nearby stations are available
    nearby_labels <- substr(stations_nearby, 1, (nchar(stations_nearby)-2))
    
    selec_obj2 <- sapply(obj2$`1day`$meta, function(m){m$sta_id %in% substr(stations_nearby, 1, nchar(stations_nearby)-2) }) 
    obj_subset2 <- obj2$`1day`$data[selec_obj2]
    
    selec_obj3 <- sapply(obj3$`1day`$meta, function(m){m$sta_id %in% substr(stations_nearby, 1, nchar(stations_nearby)-2) }) 
    obj_subset3 <- obj3$`1day`$data[selec_obj3]
    }
  


  

  # Aggregation to yearly values #


  # Spatial averaging #

obj1_average   <- average.spatial(timeseries=AWS_timeseriesselec_y) 



} # end n-loop




















# Output for  shiny overview #
BDs <- sort(unique(c(BD_y, BD_djf, BD_mam, BD_jja, BD_son)))
l <- length(BDs)
if(l < 1){
  BD_output <- NULL
}else{
  BD_output <- data.table("station(s)"=rep(compareName,l), "y"=rep("OK",l), "djf"=rep("OK",l), "mam"=rep("OK",l), "jja"=rep("OK",l), "son"=rep("OK",l) )
  BD_output$y[which(BDs %in% BD_y)] <- "Break!"
  BD_output$djf[which(BDs %in% BD_djf)] <- "Break!"
  BD_output$mam[which(BDs %in% BD_mam)] <- "Break!"
  BD_output$jja[which(BDs %in% BD_jja)] <- "Break!"
  BD_output$son[which(BDs %in% BD_son)] <- "Break!" }

if(exists("BD_complete_output")){
  rbind(BD_complete_output, BD_output)
}else{ BD_complete_output <- BD_output}
#} end of loop that calculated BD for various subsets. 

write.table(BD_complete_output, paste0("output/text/BD_output_NL_AWSvsMAN.txt"), sep=",", row.names=F, quote=F)  
