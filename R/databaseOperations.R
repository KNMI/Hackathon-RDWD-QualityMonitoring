# library(RMySQL)
# library(data.table)

#' @title Setup a connection to the MySQL database
#' @description This function returns a handle which can be used later to query the db.
#' @return An object of class "MySQLConnection" and "RMySQL"
#' @author Jurian and Hidde
db.setup <- function() {
  
  cfg <- config::get(file = "config/config.db.yml")
  
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
#' @param time.interval One of {"hour", "day", "month", "year"} NB. Season not supported ATM!
#' @param station.type One of {"validated", "derived"}
#' @param element.name Name of type of data; e.g. "rh" for hourly rainfall or "t" for temperature
#' @seealso db.setup()
#' @author Jurian and Hidde
db.select.all <- function(db, time.interval, station.type, element.name) {
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  supported.station.types <- c("validated", "derived")
  supported.time.intervals <- c("hour", "day", "month", "year")
  
  if(!station.type %in% supported.station.types) stop(paste("Unsupported station type:", station.type))
  if(!time.interval %in% supported.time.intervals) stop(paste("Unuspported time period:", time.interval))
 
  if(time.interval == "hour" | time.interval == "day") {
    time.interval <- paste0("1", time.interval)
  }
   
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  na.value <- cfg$database.na.value

  query <- sprintf (
    paste("SELECT",
          "data.data_id AS data_id, types.type_id AS type_id, elements.element_id AS element_id, stations.code AS code,",
          "DATE_FORMAT(date, %%s) AS date, value, qc, aggregation, type, name, latitude, longitude, elevation, element_group, elements.description AS element_desc, types.description AS type_desc, element, scale, unit",
          "FROM %s_%s_%s AS data",
          "INNER JOIN series ON data.data_id = series.data_id",
          "INNER JOIN stations ON series.type_id = stations.type_id AND series.code = stations.code",
          "INNER JOIN types ON series.type_id = types.type_id",
          "INNER JOIN elements ON series.element_id = elements.element_id",
          ";"
          ),
    time.interval,
    station.type,
    element.name)
  
  query.safe <- dbEscapeStrings(db, query)
  query.safe <- sprintf(query.safe, "'%Y%m%d%H%i%s'")
  result.ref <- dbSendQuery(db, query.safe)
  result <- dbFetch(result.ref, cfg$database.max.records)
  
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
  namely <- c("hourly", "daily", "monthly", "yearly")[which(time.interval == supported.time.periods)]
  
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
    
    complete.timeline <- seq(begin, end, by = time.interval)
    
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
    cfg$obj.base.id.yearly)[which(time.interval == supported.time.periods)]

  names(obj$meta) <- as.character(as.integer(names(obj$meta)) + base.id)
  names(obj[[namely]]) <- as.character(as.integer(names(obj[[namely]])) + base.id)
  
  # Clean up
  dbClearResult(result.ref)
  rm(result)
  return(obj)
}


#' @title Query the database for timeseries data and metadata
#' @param db Handle to MySQL database, taken from db.setup()
#' @param stationIDs A vector of unique station ID's (called "codes" in the DB)
#' @param typeID Unique identifier for type (e.g. 1 for "N", 2 for "H")
#' @param elementID Unique identifier for element (e.g. 1 for "RH", 2 for "RD")
#' @param series.type One of {"validated", "derived", "series"}
#' @return An object of type "mqm.data.container" which contains a list of timeseries and metadata on those series.
#' @example data.container <- db.select.timeseries(db, c(260, 324, 343, 340), 2, 1, "validated")
#' @author Jurian
#' @seealso db.setup()
db.select.timeseries <- function(db, station.IDs, type.ID, element.ID, series.type) {

  #--------------------------------------#
  ### Check the arguments for validity ###
  #--------------------------------------#
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  supported.series.types <- c("validated", "derived", "series")

  if(!series.type %in% supported.series.types) stop(paste("Unsupported series type:", station.type))
  if(length(station.IDs) == 0) stop("No station ID(s) given")
  
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  db.na.value <- cfg$database.na.value
  
  #-------------------------------------------------------------#
  ### Query the DB for timeseries info from specific stations ###
  #-------------------------------------------------------------#

  query <- sprintf(paste(
    "SELECT",
      "data_id, aggregation, element",
    "FROM",
      "series, elements",
    "WHERE",
      "series.code IN (%s) AND series.type_id = %i",
    "AND",
      "elements.element_id = series.element_id",
    "AND",
      "elements.element_id = %i",
    ";"),
    paste(station.IDs, collapse = ","),
    type.ID,
    element.ID)
  
  # Fetch from DB and store results
  result.ref <- dbSendQuery(db, dbEscapeStrings(db, query))
  result <- dbFetch(result.ref, n = -1)
  
  if(nrow(result) == 0) {
    stop("No stations match this description")
  }
  
  data.IDs <- result$data_id
  time.interval <- unique(result$aggregation)
  time.interval.db <- time.interval
  element.name <- tolower(unique(result$element))
  rm(result)
  dbClearResult(result.ref)
  
  # Fix time period references for R seq() and difftime() functions
  if(time.interval.db == "1hour" | time.interval == "1day") {
    time.interval <- substr(time.interval.db, 2, nchar(time.interval.db))
  }
  
  #---------------------------------------------#
  ### Query the DB for actual timeseries data ###
  #---------------------------------------------#
  
  query <- sprintf(paste(
    "SELECT",
      "data_id, DATE_FORMAT(date, %%s) AS datetime, value, qc",
    "FROM",
      "%s_%s_%s",
    "WHERE",
      "data_id IN (%s)",
    ";"),
    time.interval.db,
    series.type,
    element.name,
    paste(data.IDs, collapse = ","))
  
  query <- dbEscapeStrings(db, query)
  query <- sprintf(query, "'%Y%m%d%H%i%s'")
  
  result.ref <- dbSendQuery(db, query)
  result <- data.table(dbFetch(result.ref, n = cfg$database.max.records))
  setkey(result, datetime)
  dbClearResult(result.ref)
  
  #------------------------------#
  ### Create the master object ###
  #------------------------------#
  
  # Init god object...
  # ALL HAIL data.container, MASTER OF THE OBJECTS
  data.container <- list()
  class(data.container) <- cfg$data.container.main.class
  
  data.container[time.interval.db] <- list(by(result, factor(result$data_id), function(x) {
    
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
    if((difftime(end, begin, tz = "GMT", units = time.interval) + 1) > nrow(dt)) {
      complete.timeline <- data.table(datetime = format(seq(begin, end, by = time.interval), format = "%Y%m%d%H%M%S"))
      setkey(complete.timeline, datetime)
      dt <- base::merge(dt, complete.timeline, by = "datetime", all = T)
    }
    
    class(dt) <- append(class(dt), cfg$data.container.timeseries.class)
    return(dt)
  }))
  
  rm(result)
  
  #--------------------------------#
  ### Query the DB for meta data ###
  #--------------------------------#
  
  query <- sprintf(paste(
    "SELECT",
      "stations.code AS station_code,",
      "name,",
      "element,",
      "element_group,",
      "type,",
      "types.type_id AS type_id,", 
      "scale,", 
      "unit,", 
      "latitude,", 
      "longitude,",
      "elevation,", 
      "aggregation,", 
      "elements.element_id AS element_id",
    "FROM",
      "series",
    "INNER JOIN stations ON",
      "stations.code = series.code AND stations.type_id = series.type_id",
    "INNER JOIN types ON",
      "types.type_id = series.type_id",
    "INNER JOIN elements ON",
      "elements.element_id = series.element_id",
    "WHERE",
      "series.data_id IN (%s)",
    "GROUP BY",
      "data_id, station_code"
  ),paste(data.IDs, collapse = ","))
  
  result.ref <- dbSendQuery(db, dbEscapeStrings(db, query))
  result <- dbFetch(result.ref, n = -1)
  dbClearResult(result.ref)
  
  data.container$meta <- by(result, factor(data.IDs), function(x){
    
    meta <- list (
      sta_id = x$station_code,
      sta_name = tolower(x$name),
      sta_lat = x$latitude,
      sta_lon = x$longitude,
      sta_elev = x$elevation,
      sta_type = tolower(x$type),
      sta_type_id = x$type_id,
      area_sta_id = x$station_code,
      area_lat = x$latitude,
      area_lon = x$longitude,
      area_radius = 0,
      area_cat = 0,
      var_id = x$element_id,
      var_name = tolower(x$element),
      var_desc = x$element_group,
      var_unit = x$unit,
      var_scale = x$scale,
      var_period = x$aggregation,
      ser_type = series.type
    )
    class(meta) <- cfg$data.container.timeseries.meta.class
    return(meta)
  })
  
  rm(result)
  
  return(data.container)
}

#' @title Insert or update a specific timeseries based on its meta-data
#' @param db Handle to MySQL database, taken from db.setup()
#' @param meta An object of type mqm.meta.timeseries
#' @param timeseries An object of type mqm.data.timeseries, data.table of structure <datetime, value>
#' @example db.insert.update.timeseries(db, data.container$meta[<data_id>], data.container$1hour[<data_id>])
#' @author Jurian
#' @seealso db.setup()
db.insert.update.timeseries <- function(db, meta, timeseries) {
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  if(class(meta) != cfg$data.container.timeseries.meta.class) {
    stop(paste("Metadata not of class", cfg$data.container.timeseries.meta.class))
  }
  
  if(!cfg$data.container.timeseries.class %in% class(timeseries)) {
    stop(paste("Timeseries not of class", cfg$data.container.timeseries.class))
  }
  
  #-----------------------------------------------------#
  ### Determine whether this timeseries already exits ###
  #-----------------------------------------------------#
  
  query <- sprintf(paste(
      "SELECT data_id FROM", 
        "series", 
      "WHERE",
        "code = %i AND type_id = %i AND element_id = %i AND aggregation = %s",
      "LIMIT 1"), 
  meta$sta_id,
  meta$sta_type_id,
  meta$var_id,
  paste0("'", meta$var_period, "'"))
  
  result.ref <- dbSendQuery(db, query)
  result <- dbFetch(result.ref)
  dbClearResult(result.ref)
  
  do.insert <- nrow(result) == 0
  timeseries.length <- nrow(timeseries)
  
  if(do.insert) {
    
    #--------------------------------------------------------------------------#
    ### The timeseries does not yet exist. So we insert it into series table ###
    #--------------------------------------------------------------------------#
    
    # Fetch the maximum data_id from the database
    result.ref <- dbSendQuery(db, "SELECT MAX(data_id) AS max_data_id FROM series")
    new.data.ID <- dbFetch(result.ref, n = 1)$max_data_id + 1
    dbClearResult(result.ref)
    
    # Create data to insert into database
    insert.data <- data.table(
      data_id = new.data.ID,
      code = meta$sta_id, 
      type_id = meta$sta_type_id, 
      element_id = meta$var_id, 
      aggregation = paste0("'", meta$var_period,"'"))
    
    # Combine data into strings
    insert.data <- apply(insert.data, 1, function(x) {
      paste0("(", paste(x, collapse = ","), ")")
    })
    
    query <- sprintf(paste(
      "INSERT INTO",
      "series",
      "(data_id, code, type_id, element_id, aggregation)",
      "VALUES",
      "%s"
    ), insert.data)
    
    rows.affected <- dbExecute(db, query)
    
    if(rows.affected == 0) {
      stop("No series inserted into database!")
    } else {
      print(paste0("Inserted", rows.affected, "series"))
    }
    
  } else {
    
    #------------------------------------------------------------------------#
    ### The timeseries already exist. So we delete it and insert a new one ###
    #------------------------------------------------------------------------#
    
    new.data.ID <- result$data_id
    
    query <- sprintf(paste(
      "DELETE FROM",
        "TEST_%s_%s_%s",
      "WHERE",
        "data_id = %i"
    ), 
    meta$var_period,
    meta$ser_type,
    meta$var_name,
    new.data.ID)
    
    rows.affected <- dbExecute(db, query)
    
    if(rows.affected == timeseries.length) {
      print(paste("Deleted", rows.affected, "rows from the database"))
    } else {
      warning("Warning, not all records were updated")
    }
    
  }
  
  #-------------------------------#
  ### Insert the new timeseries ###
  #-------------------------------#
  
  timeseries <- data.table(data_id = new.data.ID, timeseries)
  timeseries <- apply(timeseries, 1, paste, collapse = ",")
  timeseries <- paste0("(", timeseries, ")")
  timeseries <- paste(timeseries, collapse = ",")
  
  query <- sprintf(paste(
    "INSERT INTO",
    "TEST_%s_%s_%s",
    "(data_id, date, value)",
    "VALUES",
    "%s"
  ),
  meta$var_period,
  meta$ser_type,
  meta$var_name,
  timeseries)
  
  rows.affected <- dbExecute(db, query)
  
  print(paste("Inserted", rows.affected, "rows into the database"))
 
  return(rows.affected == timeseries.length)
}
