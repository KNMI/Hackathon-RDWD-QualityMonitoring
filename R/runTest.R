source("R/aggregate.R")
source("R/dbOperations.R")

db <- setup.db()

obj <- query.hourly(db)

agg <- aggregate.to.88(obj)