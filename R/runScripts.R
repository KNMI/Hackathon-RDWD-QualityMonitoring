setwd("~/Hackathon-RDWD-QualityMonitoring/")

Sys.setenv(R_CONFIG_ACTIVE = "test")

 # Functions # 

source("R/databaseOperations.R")
source("R/aggregateOperations.R")
source("R/averagingOperations.R")
source("R/timeseriesOperations.R")
source("R/breakDetection.R")


 # Input data #

    StartTime <- proc.time()
db <- db.setup()
obj <- db.query(db, "hour", "validated", "rh")
db.close(db)
    cat(sprintf("Finished obtaining obj. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    
 # Aggregate AWS hourly values in 8-8 daily values #
    StartTime <- proc.time()
obj <- aggregate.to.88(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RH", sta_id=NULL)
    cat(sprintf("Finished Aggregating AWS hourly. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    
# Aggregate AWS and MAN daily values in yearly and seasonal values #
    StartTime <- proc.time()
obj <- aggregate.to.seasonal(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RD", sta_id=NULL) 
obj <- aggregate.to.seasonal(obj=obj, all.stations=TRUE, sta_type="MAN", var_id="RD", sta_id=NULL) 
    cat(sprintf("Finished yearly aggregating of AWS and MAN. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

    
# Average all AWS and MAN station aggregations for year and season #
    
    StartTime <- proc.time()
seriesidselec <- sapply(obj$meta,function(m){m$sta_type=="AWS" & m$var_id == "RA"})
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

seriesidselec <- sapply(obj$meta,function(m){m$sta_type=="MAN" & m$var_id == "RA"})
  seriesidlist <- names(obj$meta)[seriesidselec]
  MAN_timeseriesselec_y   <- obj$yearly$y[names(obj$yearly$y) %in% seriesidlist]
  MAN_timeseriesselec_djf <- obj$yearly$dfj[names(obj$yearly$djf) %in% seriesidlist]
  MAN_timeseriesselec_mam <- obj$yearly$mam[names(obj$yearly$mam) %in% seriesidlist]
  MAN_timeseriesselec_jja <- obj$yearly$jja[names(obj$yearly$jja) %in% seriesidlist]
  MAN_timeseriesselec_son <- obj$yearly$son[names(obj$yearly$son) %in% seriesidlist]

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
BD_y   <- break.detection(series_1=rd_AWS_MAN_y)
BD_djf <- break.detection(series_1=rd_AWS_MAN_djf)
BD_mam <- break.detection(series_1=rd_AWS_MAN_mam)
BD_jja <- break.detection(series_1=rd_AWS_MAN_jja)
BD_son <- break.detection(series_1=rd_AWS_MAN_son)
    cat(sprintf("Finished calculating break detections. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))

