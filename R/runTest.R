source("R/aggregate.R")
source("R/dbOperations.R")

db <- db.setup()

obj <- db.query.hourly(db)

db.close(db)

agg <- aggregate.to.88(obj)