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
#' @param element.name Name of type of data; e.g. "rh" for hourly rainfall or "t" for temperature
#' @seealso db.setup()
#' @author Jurian and Hidde
db.query.hourly <- function(db, element.name) {
  
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  na.value <- cfg$database.na.value
  
  table.name <- "1hour_validated_"
  
  # This is just a test example for now
  query <- sprintf (
    paste("SELECT",
          "validated.data_id AS data_id, types.type_id AS type_id, elements.element_id AS element_id, stations.code AS code,",
          "DATE_FORMAT(date, %%s) AS date, value, qc, aggregation, type, name, latitude, longitude, elevation, element_group, elements.description AS element_desc, types.description AS type_desc, element, scale, unit",
          "FROM %s%s AS validated",
          "INNER JOIN series ON validated.data_id = series.data_id",
          "INNER JOIN stations ON series.type_id = stations.type_id AND series.code = stations.code",
          "INNER JOIN types ON series.type_id = types.type_id",
          "INNER JOIN elements ON series.element_id = elements.element_id",
          ";"
          ),
    table.name, 
    element.name)
  
  query.safe <- dbEscapeStrings(db, query)
  
  query.safe <- sprintf(query.safe, "'%Y%m%d%H%i%s'")
  
  data.ref <- dbSendQuery(db, query.safe)
  
  result <- fetch(data.ref)
  
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
  
  obj$hourly <- by(result, factor(result$data_id), function(x) {
    dt <- data.table(datetime = x$date, value = x$value)
    setkey(dt, datetime)
    
    qc.idx <- !(x$qc %in% max.qc)
    missing.idx <- x$value == na.value
    dt$value[missing.idx | qc.idx] <- NA
    
    return(dt)
  })
  
  obj$daily <- list()
  obj$yearly <- list()

  names(obj$meta) <- as.character(as.integer(names(obj$meta)) + cfg$obj.base.id.hourly)
  names(obj$hourly) <- as.character(as.integer(names(obj$hourly)) + cfg$obj.base.id.hourly)
  
  # Clean up
  dbClearResult(data.ref)
  
  return(obj)
}