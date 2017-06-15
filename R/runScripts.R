setwd("~/Hackathon-RDWD-QualityMonitoring/")

 # Functions # 
source("R/databaseOperations.R")
source("R/aggregateOperations.R")
source("R/averagingOperations.R")
source("R/timeseriesOperations.R")
source("R/breakDetection.R")


 # Input data #
db <- db.setup()
obj <- db.query.hourly(db)
db.close(db)


 # Aggregate AWS hourly values in 8-8 daily values #
obj <- aggregate.to.88(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RH", sta_id=NULL)

# Aggregate AWS and MAN daily values in yearly and seasonal values #
obj <- aggregate.to.seasonal(obj=obj, all.stations=TRUE, sta_type="AWS", var_id="RD", sta_id=NULL) 
obj <- aggregate.to.seasonal(obj=obj, all.stations=TRUE, sta_type="MAN", var_id="RD", sta_id=NULL) 


# Average all AWS station aggregations #

spatial.average <- function(timeseries) 