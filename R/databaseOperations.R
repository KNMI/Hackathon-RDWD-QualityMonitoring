# library(RMySQL)
# library(data.table)
#PROBLEMS WITH OPEN CONNECTIONS?
#TRY
# (1) dbListConnections( dbDriver( drv = "MySQL"))
# (2) lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

#' @title Setup a connection to the MySQL database
#' @description This function returns a handle which can be used later to query the db.
#' @return An object of class "MySQLConnection" and "RMySQL"
#' @author Jurian and Hidde
#' @export
db.setup <- function() {
  
  cfg <- config::get(file = "~/Hackathon-RDWD-QualityMonitoring/config/config.db.yml")
  
  dbConnect(RMySQL::MySQL(), 
            dbname = cfg$dbname, 
            username = cfg$username,
            password = cfg$password, 
            host = cfg$host, 
            port = cfg$port)
}

#' @title Close the connection to the MySQL database
#' @description This closes the connection, discards all pending work, and frees resources (e.g., memory, sockets). 
#' @return TRUE, invisibly
#' @author Jurian and Hidde
#' @export
db.close <- function(db) {
  dbDisconnect(db)
}

#' @title Get station information from database
#' @description get the metadata for all stations
#' @return TRUE, invisibly
#' @author Marieke 
#' @export
station.info<-function(){
  db<-db.setup()
  query<-"SELECT * FROM stations" 
  
  query_new<-"SELECT stations.name, 
                 stations.latitude, 
                 stations.longitude, 
                 CONCAT(stations.code, '_', types.type) as code_real, 
                 stations.code, 
                 stations.type_id, 
                 elements.element,
                 start,
                 stop 
          FROM stations, types, elements, series, series_derived 
          WHERE stations.type_id=types.type_id and 
                stations.type_id=series.type_id and 
                stations.code=series.code and 
                series.element_id=elements.element_id and 
                series.data_id=series_derived.data_id ;"
  
  db.q<-dbSendQuery(db,query_new)
  results<-dbFetch(db.q,n=-1)
  
  
  dbDisconnect(db)
  
  return(results)
}

#' @title Get information from nearby stations from the database
#' @description get the metadata for all stations, input looks like code_real="260_H"
#' @return TRUE, invisibly
#' @author Marieke 
#' @export
#' 
station.nearby<-function(code_real){

split<-unlist(strsplit(code_real,"_"))
  
code=split[1]
type=split[2]


  
  db<-db.setup()
  query<-"SELECT * FROM nearby_stations"
  

  query_new<-sprintf("SELECT nearby_stations.code,nearby_stations.type_id,nearby_stations.nearby_code,nearby_stations.nearby_type_id 
                     FROM nearby_stations,types WHERE nearby_stations.type_id=types.type_id and nearby_stations.code=%s and types.type='%s';",code,type)

  db.q<-dbSendQuery(db,query_new)
  results<-dbFetch(db.q,n=-1)
  dbDisconnect(db)
  return(results)
}

#' @title Query the database for hourly 
#' @param db Handle to MySQL database, taken from db.setup()
#' @param time.period One of {"hour", "day", "month", "year"} NB. Season not supported ATM!
#' @param station.type One of {"validated", "derived"}
#' @param element.name Name of type of data; e.g. "rh" for hourly rainfall or "t" for temperature
#' @seealso db.setup()
#' @author Jurian and Hidde
db.select.all <- function(db, time.period, station.type, element.name) {
  
  supported.station.types <- c("validated", "derived")
  supported.time.periods <- c("hour", "day", "month", "year")
  
  if(!station.type %in% supported.station.types) stop(paste("Unsupported station type:", station.type))
  if(!time.period %in% supported.time.periods) stop(paste("Unuspported time period:", time.period))
  
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  na.value <- cfg$database.na.value

  query <- sprintf (
    paste("SELECT",
          "data.data_id AS data_id, types.type_id AS type_id, elements.element_id AS element_id, stations.code AS code,",
          "DATE_FORMAT(date, %%s) AS date, value, qc, aggregation, type, name, latitude, longitude, elevation, element_group, elements.description AS element_desc, types.description AS type_desc, element, scale, unit",
          "FROM 1%s_%s_%s AS data",
          "INNER JOIN series ON data.data_id = series.data_id",
          "INNER JOIN stations ON series.type_id = stations.type_id AND series.code = stations.code",
          "INNER JOIN types ON series.type_id = types.type_id",
          "INNER JOIN elements ON series.element_id = elements.element_id",
          ";"
          ),
    time.period,
    station.type,
    element.name)
  
  query.safe <- dbEscapeStrings(db, query)
  
  query.safe <- sprintf(query.safe, "'%Y%m%d%H%i%s'")
  
  data.ref <- dbSendQuery(db, query.safe)
  
  result <- dbFetch(data.ref, n = -1)
  
  obj <- list()
  class(obj) <- "mqm.data.container"
  
  obj$meta <- by(result, factor(result$data_id), function(x){
    data.table(
      sta_id = unique(x$code),
      sta_name = unique(x$name),
      sta_lat = unique(x$latitude),
      sta_lon = unique(x$longitude),
      sta_elev = unique(x$elevation),
      sta_type = ifelse(unique(x$type) == "H", "AWS", "MAN"),
      area_sta_id = unique(x$code),
      area_lat = unique(x$latitude),
      area_lon = unique(x$longitude),
      area_radius = 0,
      area_cat = 0,
      var_id = unique(x$element),
      var_name = unique(x$element_group),
      var_unit = unique(x$unit),
      var_scale = unique(x$scale),
      var_period = unique(x$aggregation),
      ser_current = x[nrow(x), "date"]
    )
  })
  
  # Init empty lists
  obj$hourly <- list()
  obj$daily <- list()
  obj$monthly <- list()
  obj$yearly <- list()
  
  # Get the right name for this list of timeseries
  namely <- c("hourly", "daily", "monthly", "yearly")[which(time.period == supported.time.periods)]
  
  obj[namely] <- list(by(result, factor(result$data_id), function(x) {
    dt <- data.table(datetime = x$date, value = x$value)
    setkey(dt, datetime)
    
    # Set any observations which do not pass the quality check to NA
    # Set any observations which are missing (-9999) to NA
    qc.idx <- !(x$qc %in% max.qc)
    missing.idx <- trunc(x$value) <= na.value
    dt$value[missing.idx | qc.idx] <- NA
    
    # Check for holes in the timeline and fill them up if necessary
    begin <- strptime(min(dt$datetime), format = "%Y%m%d%H%M%S", tz="GMT")
    end <- strptime(max(dt$datetime), format = "%Y%m%d%H%M%S", tz="GMT")
    
    complete.timeline <- seq(begin, end, by = time.period)
    
    if(length(complete.timeline) != nrow(dt)) {
      complete.timeline <- data.table( format(complete.timeline, format = "%Y%m%d%H%M%S") )
      names(complete.timeline) <- "datetime"
      setkey(complete.timeline, datetime)
      
      dt <- base::merge(dt, complete.timeline, by = "datetime", all = T)

    }
    
    return(dt)
  }))
  
  base.id <- c(
    cfg$obj.base.id.hourly, 
    cfg$obj.base.id.daily, 
    cfg$obj.base.id.monthly, 
    cfg$obj.base.id.yearly)[which(time.period == supported.time.periods)]

  names(obj$meta) <- as.character(as.integer(names(obj$meta)) + base.id)
  names(obj[[namely]]) <- as.character(as.integer(names(obj[[namely]])) + base.id)
  
  # Clean up
  dbClearResult(data.ref)
  rm(result)
 
  return(obj)
}

db.select.timeseries <- function(db, stationIDs, time.period, station.type, element.name) {
  
  #--------------------------------------#
  ### Check the arguments for validity ###
  #--------------------------------------#
  
  supported.station.types <- c("validated", "derived")
  supported.time.periods <- c("hour", "day", "month", "year")
  
  if(!station.type %in% supported.station.types) stop(paste("Unsupported station type:", station.type))
  if(!time.period %in% supported.time.periods) stop(paste("Unuspported time period:", time.period))
  if(length(stationIDs) == 0) stop("No station ID(s) given")
  
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  db.na.value <- cfg$database.na.value
  
  #-------------------------------------------------------------#
  ### Query the DB for timeseries data from specific stations ###
  #-------------------------------------------------------------#
  
  query <- sprintf(paste(
    "SELECT data.data_id AS id, DATE_FORMAT(data.date, %%s) AS datetime, data.value, data.qc",
    "FROM series",
    "INNER JOIN 1%s_%s_%s AS data ON data.data_id = series.data_id",
    "INNER JOIN stations ON ",
    "WHERE stations.code IN (%s) "),
    time.period,
    station.type,
    element.name,
    paste(stationIDs, collapse = ","))
  
  query.safe <- dbEscapeStrings(db, query)
  query.safe <- sprintf(query.safe, "'%Y%m%d%H%i%s'")
  data.ref <- dbSendQuery(db, query.safe)
  
  result <- as.data.table(dbFetch(data.ref, n = -1))
  setkey(result, datetime)
  
  # Clean up
  dbClearResult(data.ref)
  
  #------------------------------#
  ### Create the master object ###
  #------------------------------#
  
  # Init god object...
  # ALL HAIL OBJ, MASTER OF THE OBJECTS
  obj <- list()
  class(obj) <- cfg$obj.main.class
  
  # Init empty lists
  obj$hourly <- list()
  obj$daily <- list()
  obj$monthly <- list()
  obj$yearly <- list()
  
  # Get the right name for this list of timeseries
  namely <- c("hourly", "daily", "monthly", "yearly")[which(time.period == supported.time.periods)]
  
  obj[namely] <- list(by(result, factor(result$id), function(x) {
    
    dt <- data.table(datetime = x$datetime, value = x$value)
    setkey(dt, datetime)
    
    # Set any observations which do not pass the quality check to NA
    # Set any observations which are missing (-9999) to NA
    qc.idx <- !(x$qc %in% max.qc)
    missing.idx <- trunc(x$value) <= db.na.value
    dt$value[missing.idx | qc.idx] <- NA
    
    # We need the begin and end of the timeseries to check for holes
    begin <- strptime(min(dt$datetime), format = "%Y%m%d%H%M%S", tz = "GMT")
    end <- strptime(max(dt$datetime), format = "%Y%m%d%H%M%S", tz = "GMT")
    
    # If the timeseries has holes, then fill them up with NA's
    if((difftime(end, begin, tz = "GMT", units = time.period) + 1) > nrow(dt)) {
      complete.timeline <- data.table( datetime = format(seq(begin, end, by = time.period), format = "%Y%m%d%H%M%S") )
      setkey(complete.timeline, datetime)
      dt <- base::merge(dt, complete.timeline, by = "datetime", all = T)
    }
    
    class(dt) <- append(class(dt), cfg$obj.timeseries.class)
    return(dt)
  }))
  
  #--------------------------------#
  ### Query the DB for meta data ###
  #--------------------------------#
  
  query <- sprintf(paste(
    "SELECT",
    "series.data_id AS id, stations.code AS station_code, name,",
    "types.type_id AS type_id, elements.element_id AS element_id,",
    "element_group, type, scale, unit, types.description AS type_desc,",
    "latitude, longitude, elevation, aggregation, period, element",
    "FROM series",
    "INNER JOIN stations ON stations.code = series.code AND stations.type_id = series.type_id",
    "INNER JOIN types ON types.type_id = series.type_id",
    "INNER JOIN elements ON elements.element_id = series.element_id",
    "WHERE series.data_id IN (%s)"
  ),paste(seriesID, collapse = ","))
  
  query.safe <- dbEscapeStrings(db, query)
  data.ref <- dbSendQuery(db, query.safe)
  result <- dbFetch(data.ref, n = -1)
  
  dbClearResult(data.ref)
  
  obj$meta <- by(result, factor(result$id), function(x){
    
    meta <- list (
      sta_id = unique(x$station_code),
      sta_name = unique(x$name),
      sta_lat = unique(x$latitude),
      sta_lon = unique(x$longitude),
      sta_elev = unique(x$elevation),
      sta_type = ifelse(unique(x$type) == "H", "AWS", "MAN"),
      area_sta_id = unique(x$station_code),
      area_lat = unique(x$latitude),
      area_lon = unique(x$longitude),
      area_radius = 0,
      area_cat = 0,
      var_id = unique(x$element),
      var_name = unique(x$element_group),
      var_unit = unique(x$unit),
      var_scale = unique(x$scale),
      var_period = unique(x$aggregation)
      #ser_current = x[nrow(x), "date"]
    )
    class(meta) <- cfg$obj.timeseries.meta.class
    return(meta)
  })
  

  # dbDisconnect(db)
  return(obj)
}

db.insert.timeseries <- function(db, timeseries, time.period, station.type, element.name) {
  
}

# db.update.timeseries <- function(db) {
#   
# }
