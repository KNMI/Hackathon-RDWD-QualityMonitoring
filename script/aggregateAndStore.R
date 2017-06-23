Sys.setenv(R_CONFIG_ACTIVE = "test")
source("R/databaseOperations.R")
source("R/aggregateOperations.R")

obj <- db.execute(db.select.all, time.interval="1hour", type="H", element="RH")  # AWS
obj <- aggregate.to.88.2(obj)
obj <- aggregate.to.seasonal.2(obj)
obj <- aggregateTo.year(obj) 
obj2 <- db.execute(db.select.all, time.interval="1day", type="N", element="RD")  # MAN
obj2 <- aggregate.to.seasonal.2(obj2)
obj2 <- aggregateTo.year(obj2) 
obj3 <- db.execute(db.select.all, time.interval="1hour", type="N", element="RR") # Radar at MAN locations
obj3 <- aggregate.to.88.2(obj3)
obj3 <- aggregate.to.seasonal.2(obj3)
obj3 <- aggregateTo.year(obj3)
obj4 <- db.execute(db.select.all, time.interval="1hour", type="H", element="RR") # Radar at AWS locations
obj4 <- aggregate.to.88.2(obj4)
obj4 <- aggregate.to.seasonal.2(obj4)
obj4 <- aggregateTo.year(obj4)


db <- db.setup()

#------------------------------------------------------#
### Save the daily aggregations back to the database ###
#------------------------------------------------------#

obj.result <- sapply(names(obj$`1day`$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj$`1day`$meta[[data.ID]],
    obj$`1day`$data[[data.ID]])
  
})

obj3.result <- sapply(names(obj3$`1day`$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj3$`1day`$meta[[data.ID]],
    obj3$`1day`$data[[data.ID]])
  
})

obj4.result <- sapply(names(obj4$`1day`$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj4$`1day`$meta[[data.ID]],
    obj4$`1day`$data[[data.ID]])
  
})

#---------------------------------------------------------#
### Save the seasonal aggregations back to the database ###
#---------------------------------------------------------#

obj.result <- sapply(names(obj$season$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj$season$meta[[data.ID]],
    obj$season$data[[data.ID]])
  
})

obj2.result <- sapply(names(obj2$season$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj2$season$meta[[data.ID]],
    obj2$season$data[[data.ID]])
  
})

obj3.result <- sapply(names(obj3$season$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj3$season$meta[[data.ID]],
    obj3$season$data[[data.ID]])
  
})

obj4.result <- sapply(names(obj4$season$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj4$season$meta[[data.ID]],
    obj4$season$data[[data.ID]])
  
})

#-------------------------------------------------------#
### Save the yearly aggregations back to the database ###
#-------------------------------------------------------#

obj.result <- sapply(names(obj$year$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj$year$meta[[data.ID]],
    obj$year$data[[data.ID]])
  
})

obj2.result <- sapply(names(obj2$year$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj2$year$meta[[data.ID]],
    obj2$year$data[[data.ID]])
  
})

obj3.result <- sapply(names(obj3$year$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj3$year$meta[[data.ID]],
    obj3$year$data[[data.ID]])
  
})

obj4.result <- sapply(names(obj4$year$meta), function(data.ID){
  
  db.insert.update.timeseries (
    db,
    obj4$year$meta[[data.ID]],
    obj4$year$data[[data.ID]])
  
})

db.close(db)
