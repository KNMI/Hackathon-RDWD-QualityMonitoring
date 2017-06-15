source("R/aggregateOperations.R")
source("R/databaseOperations.R")

db <- db.setup()

obj <- db.query.hourly(db, "rh")

db.close(db)

agg <- aggregate.to.88(obj)