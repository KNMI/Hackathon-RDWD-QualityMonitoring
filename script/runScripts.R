setwd("~/Hackathon-RDWD-QualityMonitoring/")

Sys.setenv(R_CONFIG_ACTIVE = "test")

 # Functions # 
<<<<<<< HEAD
source("R/databaseOperations.R")
source("R/aggregateOperations.R")
source("R/averagingOperations.R")
source("R/timeseriesOperations.R")
source("R/breakDetection.R")
=======
library(R.utils)
sourceDirectory("R")
# source("R/databaseOperations.R")
# source("R/aggregateOperations.R")
# source("R/averagingOperations.R")
# source("R/timeseriesOperations.R")
# source("R/breakDetection.R")
>>>>>>> Development


 # Input data #

    StartTime <- proc.time()
db <- db.setup()
<<<<<<< HEAD
obj <- db.select.all(db, "1day", 2, 1, "rh", "validated")
obj2 <- db.select.all(db, "1day", 2, 1, "rd", "validated")
#obj <- db.query(db, "hour", "validated", "rh")
#obj2 <- db.query(db, "day", "derived", "rd")
=======
obj <- db.select.all(db, "1day", "N", "RD")
obj2 <- db.select.all(db, "day", "derived", "rd")
>>>>>>> Development
db.close(db)
    cat(sprintf("Finished obtaining obj. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))
    
 # Define the list of sta_id that you want to process #    
#sta_id <-   #output of query range selection (made by Marieke)/ standard list of related stations from database    
  # to be filled in once the exact formulation of those outputs is known. 
  # Possible output of range selection is all station id's with distance to station that is selected. In that case add line that selects only the stations where this distance is shorter than a given range. 
  
 # Aggregate AWS hourly values in 8-8 daily values #
    StartTime <- proc.time()
obj <- aggregateTo.88(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RH", sta_id=sta_id)
    cat(sprintf("Finished Aggregating AWS hourly. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    
# Aggregate AWS and MAN daily values in yearly and seasonal values #
    StartTime <- proc.time()
obj <- aggregateTo.seasonal(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RD", sta_id=sta_id) 
obj2 <- aggregateTo.seasonal(obj=obj2, all.stations=TRUE, sta_type="MAN", var_id="RD", sta_id=sta_id) 
    cat(sprintf("Finished yearly aggregating of AWS and MAN. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    
# Average all AWS and MAN station aggregations for year and season #

        
    StartTime <- proc.time()
seriesidselec <- sapply(obj$meta,function(m){m$sta_type=="AWS" & m$var_id == "RA"})
#seriesidselec <- sapply(obj$meta,function(m){m$sta_type=="AWS" & m$var_id == "RA", sta_id=sta_id}) # line will be used when sta_id is identified. 
    seriesidlist <- names(obj$meta)[seriesidselec]
  AWS_timeseriesselec_y   <- obj$yearly$y[names(obj$yearly$y) %in% seriesidlist]
  AWS_timeseriesselec_djf <- obj$yearly$djf[names(obj$yearly$djf) %in% seriesidlist]
  AWS_timeseriesselec_mam <- obj$yearly$mam[names(obj$yearly$mam) %in% seriesidlist]
  AWS_timeseriesselec_jja <- obj$yearly$jja[names(obj$yearly$jja) %in% seriesidlist]
  AWS_timeseriesselec_son <- obj$yearly$son[names(obj$yearly$son) %in% seriesidlist]

  AWS_average_y   <- average.spatial(timeseries=AWS_timeseriesselec_y) 
  AWS_average_djf <- average.spatial(timeseries=AWS_timeseriesselec_djf) 
  AWS_average_mam <- average.spatial(timeseries=AWS_timeseriesselec_mam) 
  AWS_average_jja <- average.spatial(timeseries=AWS_timeseriesselec_jja) 
  AWS_average_son <- average.spatial(timeseries=AWS_timeseriesselec_son) 

seriesidselec <- sapply(obj2$meta,function(m){m$sta_type=="MAN" & m$var_id == "RA"})
#seriesidselec <- sapply(obj2$meta,function(m){m$sta_type=="MAN" & m$var_id == "RA", sta_id=sta_id})# line will be used when sta_id is identified. 


  seriesidlist <- names(obj2$meta)[seriesidselec]
  MAN_timeseriesselec_y   <- obj2$yearly$y[names(obj2$yearly$y) %in% seriesidlist]
  MAN_timeseriesselec_djf <- obj2$yearly$djf[names(obj2$yearly$djf) %in% seriesidlist]
  MAN_timeseriesselec_mam <- obj2$yearly$mam[names(obj2$yearly$mam) %in% seriesidlist]
  MAN_timeseriesselec_jja <- obj2$yearly$jja[names(obj2$yearly$jja) %in% seriesidlist]
  MAN_timeseriesselec_son <- obj2$yearly$son[names(obj2$yearly$son) %in% seriesidlist]

  MAN_average_y   <- average.spatial(timeseries=MAN_timeseriesselec_y) 
  MAN_average_djf <- average.spatial(timeseries=MAN_timeseriesselec_djf) 
  MAN_average_mam <- average.spatial(timeseries=MAN_timeseriesselec_mam) 
  MAN_average_jja <- average.spatial(timeseries=MAN_timeseriesselec_jja) 
  MAN_average_son <- average.spatial(timeseries=MAN_timeseriesselec_son) 
    cat(sprintf("Finished spatial aggregating of AWS and MAN. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))  

    
  # Calculate relative difference of AWS and MAN for year and season #

    StartTime <- proc.time()  
rd_AWS_MAN_y   <- timeseries.relative.difference(timeserie1=AWS_average_y, timeserie2=MAN_average_y)
rd_AWS_MAN_djf <- timeseries.relative.difference(timeserie1=AWS_average_djf, timeserie2=MAN_average_djf)
rd_AWS_MAN_mam <- timeseries.relative.difference(timeserie1=AWS_average_mam, timeserie2=MAN_average_mam)
rd_AWS_MAN_jja <- timeseries.relative.difference(timeserie1=AWS_average_jja, timeserie2=MAN_average_jja)
rd_AWS_MAN_son <- timeseries.relative.difference(timeserie1=AWS_average_son, timeserie2=MAN_average_son)
    cat(sprintf("Finished calculated relative difference of AWS and MAN. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    
 # Break detections #
    
    StartTime <- proc.time()  
BD_y   <- break.detection(series1=rd_AWS_MAN_y)
BD_djf <- break.detection(series1=rd_AWS_MAN_djf)
BD_mam <- break.detection(series1=rd_AWS_MAN_mam)
BD_jja <- break.detection(series1=rd_AWS_MAN_jja)
BD_son <- break.detection(series1=rd_AWS_MAN_son)
    cat(sprintf("Finished calculating break detections. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    #comparename should be identified automatically where the choises are made concerning which subset is compared with which other subset. 
    #In this case a name for all AWS vs all MAN in NL are considered. 
    #If multiple comparisons are done, loop the BD calculation selection and rbind the BD output. A different compareName should be constructed for each loop.     
compareName <-  "NL_AWSvsMAN"   # temporary compareName. 
  
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
      
  # Visualisation # 
png(paste0("output/fig/hackathon_",compareName,"_y.png"))
plot(rd_AWS_MAN_y$datetime, rd_AWS_MAN_y$value, type="l", xlab="Time", ylab="Relative difference [%]", main="Yearly AWS vs MAN")
points(rd_AWS_MAN_y$datetime, rd_AWS_MAN_y$value, pch=20)
abline(v=BD_y, lty=2)
abline(h=0, col="grey")
dev.off()
  
png(paste0("output/fig/hackathon_",compareName,"_djf.png"))
plot(rd_AWS_MAN_djf$datetime, rd_AWS_MAN_djf$value, type="l", xlab="Time", ylab="Relative difference [%]", main="Winter AWS vs MAN", col="red")
points(rd_AWS_MAN_djf$datetime, rd_AWS_MAN_djf$value, pch=20, col="red")
abline(v=BD_djf, lty=2, col="red")
abline(h=0, col="grey")
dev.off()

png(paste0("output/fig/hackathon_",compareName,"_mam.png"))
plot(rd_AWS_MAN_mam$datetime, rd_AWS_MAN_mam$value, type="l", xlab="Time", ylab="Relative difference [%]", main="Spring AWS vs MAN", col="blue")
points(rd_AWS_MAN_mam$datetime, rd_AWS_MAN_mam$value, pch=20, col="blue")
abline(v=BD_mam, lty=2, col="blue")
abline(h=0, col="grey")
dev.off()

png(paste0("output/fig/hackathon_",compareName,"_jja.png"))
plot(rd_AWS_MAN_jja$datetime, rd_AWS_MAN_jja$value, type="l", xlab="Time", ylab="Relative difference [%]", main="Summer AWS vs MAN", col="violet")
points(rd_AWS_MAN_jja$datetime, rd_AWS_MAN_jja$value, pch=20, col="violet")
abline(v=BD_jja, lty=2, col="violet")
abline(h=0, col="grey")
dev.off()
        
png(paste0("output/fig/hackathon_",compareName,"_son.png"))
plot(rd_AWS_MAN_son$datetime, rd_AWS_MAN_son$value, type="l", xlab="Time", ylab="Relative difference [%]", main="Autumn AWS vs MAN: ", col="green")
points(rd_AWS_MAN_son$datetime, rd_AWS_MAN_son$value, pch=20, col="green")
abline(v=BD_son, lty=2, col="green")
abline(h=0, col="grey")
dev.off()

png(paste0("output/fig/hackathon_",compareName,"_all.png"))
plot(rd_AWS_MAN_y$datetime, rd_AWS_MAN_y$value, type="l", xlab="Time", ylab="Relative difference [%]", main="AWS vs MAN")
points(rd_AWS_MAN_y$datetime, rd_AWS_MAN_y$value, pch=20)
abline(h=0, col="grey")
abline(v=BD_y, lty=2)

lines(rd_AWS_MAN_djf$datetime, rd_AWS_MAN_djf$value, col="red")
points(rd_AWS_MAN_djf$datetime, rd_AWS_MAN_djf$value, pch=20, col="red")
abline(v=BD_djf, lty=2, col="red")

lines(rd_AWS_MAN_mam$datetime, rd_AWS_MAN_mam$value, col="blue")
points(rd_AWS_MAN_mam$datetime, rd_AWS_MAN_mam$value, pch=20, col="blue")
abline(v=BD_mam, lty=2, col="blue")

lines(rd_AWS_MAN_jja$datetime, rd_AWS_MAN_jja$value, col="violet")
points(rd_AWS_MAN_jja$datetime, rd_AWS_MAN_jja$value, pch=20, col="violet")
abline(v=BD_jja, lty=2, col="violet")

lines(rd_AWS_MAN_son$datetime, rd_AWS_MAN_son$value, type="l", col="green")
points(rd_AWS_MAN_son$datetime, rd_AWS_MAN_son$value, pch=20, col="green")
abline(v=BD_son, lty=2, col="green")

legend("bottomright", c("year", "winter", "spring", "summer", "autumn"), col=c("black", "red", "blue", "violet", "green"), pch=20)

dev.off()
