library(RMySQL)
library(data.table)

#' @title Setup a connection to the MySQL database
#' @description This function returns a handle which can be used later to query the db.
#' @return An object of class "MySQLConnection" and "RMySQL"
#' @author Jurian and Hidde
db.setup <- function() {
  
  cfg <- config::get(file = "config/config.yml")
  
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
db.close <- function(db) {
  dbDisconnect(db)
}

#' @title Query the database for hourly 
#' @param db Handle to MySQL database, taken from db.setup()
#' @param time.period One of {"hour", "day", "month", "year"} NB. Season not supported ATM!
#' @param station.type One of {"validated", "derived"}
#' @param element.name Name of type of data; e.g. "rh" for hourly rainfall or "t" for temperature
#' @seealso db.setup()
#' @author Jurian and Hidde
db.query <- function(db, time.period, station.type, element.name) {
  
  supported.station.types <- c("validated", "derived")
  supported.time.periods <- c("hour", "day", "month", "year")
  
  if(!station.type %in% supported.station.types) stop(paste("Unsupported station type:", station.type))
  if(!time.period %in% supported.time.periods) stop(paste("Unuspported time period:", time.period))
  
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  na.value <- cfg$database.na.value

  query <- sprintf (
    paste("SELECT",
          "validated.data_id AS data_id, types.type_id AS type_id, elements.element_id AS element_id, stations.code AS code,",
          "DATE_FORMAT(date, %%s) AS date, value, qc, aggregation, type, name, latitude, longitude, elevation, element_group, elements.description AS element_desc, types.description AS type_desc, element, scale, unit",
          "FROM 1%s_%s_%s AS validated",
          "INNER JOIN series ON validated.data_id = series.data_id",
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
  
  result <- fetch(data.ref, n = -1)
  
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
    
    qc.idx <- !(x$qc %in% max.qc)
    missing.idx <- x$value == na.value
    dt$value[missing.idx | qc.idx] <- NA
    
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