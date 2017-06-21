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

label_list <-  c("NL", etc...)    # AWS name list e.g. 260_H
                  
for(label in label_list){
  singleAWS ## based on database query reads which stations are related to this
  related_MAN ## based on database query reads which stations are related to this 
  
  # Input data #
  if(label == "NL"){
    db <- db.setup()
    subset1 <- db.select.all(db, "hour", "validated", "rh")         # list of time series data.tables, dependent on label.  
    subset2 <- db.select.all(db, "day", "derived", "rd")  
  }else{    
    db <- db.setup()
    subset1 <- db.select.timeseries(db, stationIDs=singleAWS, typeID="H", elementID=1, series.type="validated")          # list of time series data.tables, dependent on label.  
    subset2 <- db.select.timeseries(db, stationIDs=related_MAN, typeID="N", elementID=2, series.type="derived")    
    db.close(db)
  }
   
# notes from else branch.   
  #' @title Query the database for timeseries data and metadata
  #' @param db Handle to MySQL database, taken from db.setup()
  #' @param stationIDs A vector of unique station ID's (called "codes" in the DB)
  #' @param typeID Unique identifier for type (e.g. 1 for "N", 2 for "H")
  #' @param elementID Unique identifier for element (e.g. 1 for "RH", 2 for "RD")
  #' @param series.type One of {"validated", "derived", "series"}
  #' @return An object of type "mqm.data.container" which contains a list of timeseries and metadata on those series.
  #' @example data.container <- db.select.timeseries(db, c(260, 324, 343, 340), 2, 1, "validated")
  #' @author Jurian
  #' @seealso db.setup()
  db.select.timeseries <-
  
   
  StartTime <- proc.time()
  db <- db.setup()
  obj <- db.select.all(db, "hour", "validated", "rh")
  obj2 <- db.select.all(db, "day", "derived", "rd")
  db.close(db)
  cat(sprintf("Finished obtaining obj. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))
  
  
}                  

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
