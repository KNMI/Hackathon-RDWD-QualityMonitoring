setwd("~/Hackathon-RDWD-QualityMonitoring/")

 # Functions # 
source("R/databaseOperations.R")
source("R/aggregateOperations.R")
source("R/averagingOperations.R")
source("R/timeseriesOperations.R")
source("R/breakDetection.R")


 # Input data #
db <- db.setup()
obj <- db.query.hourly(db, "rh")
db.close(db)


 # Aggregate AWS hourly values in 8-8 daily values #
obj <- aggregate.to.88(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RH", sta_id=NULL)

# Aggregate AWS and MAN daily values in yearly and seasonal values #
obj <- aggregate.to.seasonal(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RD", sta_id=NULL) 
obj <- aggregate.to.seasonal(obj=obj, all.stations=TRUE, sta_type="MAN", var_id="RD", sta_id=NULL) 


# Average all AWS and MAN station aggregations for year and season #

seriesidselec <- sapply(obj$meta,function(m){m$sta_type=="AWS" & m$var_id == "RA"})
  seriesidlist <- names(obj$meta)[seriesidselec]
  AWS_timeseriesselec_y <- obj$yearly$y[names(obj$yearly$y) %in% seriesidlist]
  AWS_timeseriesselec_djf <- obj$yearly$dfj[names(obj$yearly$djf) %in% seriesidlist]
  AWS_timeseriesselec_mam <- obj$yearly$mam[names(obj$yearly$mam) %in% seriesidlist]
  AWS_timeseriesselec_jja <- obj$yearly$jja[names(obj$yearly$jja) %in% seriesidlist]
  AWS_timeseriesselec_son <- obj$yearly$son[names(obj$yearly$son) %in% seriesidlist]

  AWS_average_y <- spatial.average(timeseries=AWS_timeseriesselec_y) 
  AWS_average_djf <- spatial.average(timeseries=AWS_timeseriesselec_djf) 
  AWS_average_mam <- spatial.average(timeseries=AWS_timeseriesselec_mam) 
  AWS_average_jja <- spatial.average(timeseries=AWS_timeseriesselec_jja) 
  AWS_average_son <- spatial.average(timeseries=AWS_timeseriesselec_son) 

seriesidselec <- sapply(obj$meta,function(m){m$sta_type=="MAN" & m$var_id == "RA"})
  seriesidlist <- names(obj$meta)[seriesidselec]
  MAN_timeseriesselec_y <- obj$yearly$y[names(obj$yearly$y) %in% seriesidlist]
  MAN_timeseriesselec_djf <- obj$yearly$dfj[names(obj$yearly$djf) %in% seriesidlist]
  MAN_timeseriesselec_mam <- obj$yearly$mam[names(obj$yearly$mam) %in% seriesidlist]
  MAN_timeseriesselec_jja <- obj$yearly$jja[names(obj$yearly$jja) %in% seriesidlist]
  MAN_timeseriesselec_son <- obj$yearly$son[names(obj$yearly$son) %in% seriesidlist]

  MAN_average_y <- spatial.average(timeseries=MAN_timeseriesselec_y) 
  MAN_average_djf <- spatial.average(timeseries=MAN_timeseriesselec_djf) 
  MAN_average_mam <- spatial.average(timeseries=MAN_timeseriesselec_mam) 
  MAN_average_jja <- spatial.average(timeseries=MAN_timeseriesselec_jja) 
  MAN_average_son <- spatial.average(timeseries=MAN_timeseriesselec_son) 
  

  # Calculate relative difference for year and season #
  
rd_AWS_MAN_Y <- timeseries.relative.difference(timeserie1, timeserie2)
  