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
#' @param db Data base
db.close <- function(db) {
  dbDisconnect(db)
}

#' @title Query the database for hourly 
#' @param db Handle to MySQL database, taken from db.setup()
#' @param time.interval One of {"1hour", "1day", "month", "season, "year"}
#' @param type One of {"N" (Manual), "H" (Automatic)} Case insensitive
#' @param element One of {"RH" (Precipitation, originated from hourly data), "RD" (Precipitation, from daily data), "RR" (Precipitation, from radar data)} Case insensitive
#' #@example data.container <- db.select.all(db, "1hour", "N", "RH") 
#' @seealso db.setup()
#' @description a function
#' @author Jurian and Hidde
db.select.all <- function(db, time.interval, type, element) {
  
  #--------------------------------------#
  ### Check the arguments for validity ###
  #--------------------------------------#
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  supported.time.intervals <- c("1hour", "1day", "month", "season", "year")
  if(!time.interval %in% supported.time.intervals) stop(paste("Unsupported time interval:", time.interval))
  
  cfg <- config::get(file = "config/config.yml")
  max.qc <- cfg$qc.threshold
  db.na.value <- cfg$database.na.value
  
  type <- tolower(type)
  element <- tolower(element)
  
  # Find the correct element ID and type ID in the database
  ref <- dbSendQuery(db, sprintf(
    "SELECT type_id, element_id FROM types, elements WHERE type = %s AND element = %s;", paste0("'", type, "'"), paste0("'", element, "'")))
  type.element <- dbFetch(ref, n = 1)
  dbClearResult(ref)
  
  if(nrow(type.element) == 0) {
    stop("Error finding the correct type and/or element in the database")
  }
  
  type.ID <- type.element$type_id
  element.ID <- type.element$element_id
  rm(type.element)
  
  time.interval.db <- time.interval
  # Fix time interval references for R seq() and difftime() functions
  if(time.interval.db == "1hour" | time.interval.db == "1day") {
    time.interval <- substr(time.interval.db, 2, nchar(time.interval.db))
  }
  
  data.container <- list()
  class(data.container) <- "mqm.data.container"
  data.container[[time.interval.db]] <- list()
  
  query <- sprintf(paste(
    "SELECT",
    "series.data_id AS data_id,",
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
    "INNER JOIN series_derived ON",
    "series.data_id = series_derived.data_id",
    "WHERE",
    "series.type_id = %i AND series.element_id = %i AND series.aggregation = %s",
    "GROUP BY",
    "data_id, station_code"
  ),
  type.ID,
  element.ID,
  paste0("'", time.interval.db, "'"))
  
  result.ref <- dbSendQuery(db, query)
  result <- dbFetch(result.ref, cfg$database.max.records)
  dbClearResult(result.ref)
  
  data.container[[time.interval.db]]$meta <- by(result, factor(result$station_code), function(x) {
    
    meta <- list (
      dat_id = x$data_id,
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
      var_interval = x$aggregation
    )
    class(meta) <- cfg$data.container.timeseries.meta.class
    return(meta)
  })
  
  if(nrow(result) == 0) {
    stop("No stations match this description")
  }
  
  data.IDs <- sapply(data.container[[time.interval.db]]$meta, function(x) {x$dat_id})
  names(data.container[[time.interval.db]]$meta) <- data.IDs
  
  query <- sprintf(paste(
    "SELECT",
    "data_id, DATE_FORMAT(date, %%s) AS datetime, value, qc",
    "FROM",
    "%s_series_%s",
    "WHERE",
    "data_id IN (%s)"
  ),
  time.interval.db,
  element,
  paste(data.IDs, collapse = ","))
  
  query <- sprintf(query, "'%Y%m%d%H%i%s'")
  
  result.ref <- dbSendQuery(db, query)
  result <- dbFetch(result.ref, cfg$database.max.records)
  dbClearResult(result.ref)
  
  
  data.container[[time.interval.db]]$data <- by(result, factor(result$data_id), function(x) {
    
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
  })
  
  names(data.container[[time.interval.db]]$data) <- data.IDs
  
  rm(result)
  return(data.container)
}


#' @title Query the database for timeseries data and metadata
#' @param db Handle to MySQL database, taken from db.setup()
#' @param station.IDs A vector of unique station ID's (called "codes" in the DB)
#' @param time.interval One of {"1hour", "1day", "month", "season, "year"}
#' @param type One of {"N", "H"} (case insensitive)
#' @param element One of {"RH", "RD", "RR"} (case insensitive)
#' @return An object of type "mqm.data.container" which contains a list of timeseries and metadata on those series.
#' #@example data.container <- db.select.timeseries(db, c(260, 324, 343, 340), "1hour", "H", "RH")
#' @author Jurian
#' @description a function
#' @seealso db.setup()
db.select.timeseries <- function(db, station.IDs, time.interval, type, element) {
  
  #--------------------------------------#
  ### Check the arguments for validity ###
  #--------------------------------------#
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  if(length(station.IDs) == 0) stop("No station ID(s) given")
  
  supported.time.intervals <- c("1hour", "1day", "month", "season", "year")
  if(!time.interval %in% supported.time.intervals) stop(paste("Unsupported time interval:", time.interval))
  
  #-------------------------------------------------------------#
  ### Find the correct element ID and type ID in the database ###
  #--------------------------------------------------------------#
  
  type <- tolower(type)
  element <- tolower(element)
  
  ref <- dbSendQuery(db, sprintf(
    "SELECT type_id, element_id FROM types, elements WHERE type = %s AND element = %s;", paste0("'", type, "'"), paste0("'", element, "'")))
  type.element <- dbFetch(ref, n = 1)
  dbClearResult(ref)
  
  if(nrow(type.element) == 0) {
    stop("Error finding the correct type and/or element in the database")
  }
  
  type.ID <- type.element$type_id
  element.ID <- type.element$element_id
  rm(type.element)
  
  cfg <- config::get(file = "config/config.yml")
  
  max.qc <- cfg$qc.threshold
  db.na.value <- cfg$database.na.value
  
  time.interval.db <- time.interval
  # Fix time interval references for R seq() and difftime() functions
  if(time.interval.db == "1hour" | time.interval.db == "1day") {
    time.interval <- substr(time.interval.db, 2, nchar(time.interval.db))
  }
  
  #-------------------------------------------------------------#
  ### Query the DB for timeseries info from specific stations ###
  #-------------------------------------------------------------#
  
  query <- sprintf(paste(
    "SELECT",
    "series.data_id AS data_id, aggregation, element",
    "FROM",
    "series, elements, series_derived",
    "WHERE",
    "series.code IN (%s) AND series.type_id = %i",
    "AND",
    "elements.element_id = series.element_id",
    "AND",
    "series.data_id = series_derived.data_id",
    "AND",
    "elements.element_id = %i",
    "AND",
    "series.aggregation = %s",
    ";"),
    paste(station.IDs, collapse = ","),
    type.ID,
    element.ID,
    paste0("'", time.interval.db, "'"))
  
  # Fetch from DB and store results
  result.ref <- dbSendQuery(db, query)
  result <- dbFetch(result.ref, n = -1)
  
  if(nrow(result) == 0) {
    stop("No stations match this description")
  }
  
  data.IDs <- result$data_id
  time.interval.db <- unique(result$aggregation)
  element.name <- tolower(unique(result$element))
  rm(result)
  dbClearResult(result.ref)
  
  #------------------------------#
  ### Create the master object ###
  #------------------------------#
  
  # Init god object...
  # ALL HAIL data.container, MASTER OF THE OBJECTS
  data.container <- list()
  class(data.container) <- cfg$data.container.main.class
  data.container[[time.interval.db]] <- list()
  
  #--------------------------------#
  ### Query the DB for meta data ###
  #--------------------------------#
  
  query <- sprintf(paste(
    "SELECT",
    "series.data_id AS data_id,",
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
    "INNER JOIN series_derived ON",
    "series.data_id = series_derived.data_id",
    "WHERE",
    "series.data_id IN (%s)",
    "GROUP BY",
    "data_id, station_code"
  ),paste(data.IDs, collapse = ","))
  
  result.ref <- dbSendQuery(db, dbEscapeStrings(db, query))
  result <- dbFetch(result.ref, n = -1)
  dbClearResult(result.ref)
  
  data.container[[time.interval.db]]$meta <- by(result, factor(data.IDs), function(x) {
    
    meta <- list (
      dat_id = x$data_id,
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
      var_interval = x$aggregation
    )
    class(meta) <- cfg$data.container.timeseries.meta.class
    return(meta)
  })
  
  names(data.container[[time.interval.db]]$meta) <- sapply(data.container[[time.interval.db]]$meta, function(x){x$dat_id})
  rm(result)
  
  #---------------------------------------------#
  ### Query the DB for actual timeseries data ###
  #---------------------------------------------#
  
  query <- sprintf(paste(
    "SELECT",
    "data_id, DATE_FORMAT(date, %%s) AS datetime, value, qc",
    "FROM",
    "%s_series_%s",
    "WHERE",
    "data_id IN (%s)",
    ";"),
    time.interval.db,
    element.name,
    paste(data.IDs, collapse = ","))
  
  query <- dbEscapeStrings(db, query)
  query <- sprintf(query, "'%Y%m%d%H%i%s'")
  
  result.ref <- dbSendQuery(db, query)
  result <- data.table(dbFetch(result.ref, n = cfg$database.max.records))
  setkey(result, datetime)
  dbClearResult(result.ref)
  
  data.container[[time.interval.db]]$data <- by(result, factor(data.IDs), function(x) {
    
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
  })
  names(data.container[[time.interval.db]]$data) <- sapply(data.container[[time.interval.db]]$meta, function(x){x$dat_id})
  rm(result)
  
  return(data.container)
}

#' @title Insert or update a specific timeseries based on its meta-data
#' @param db Handle to MySQL database, taken from db.setup()
#' @param meta An object of type mqm.meta.timeseries
#' @param timeseries An object of type mqm.data.timeseries, data.table of structure <datetime, value>
#' #@example db.insert.update.timeseries(db, data.container$meta[<data_id>], data.container$1hour[<data_id>])
#' @description a function
#' @author Jurian
#' @seealso db.setup()
#' @export
db.insert.update.timeseries <- function(db, meta, timeseries) {
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  cfg <- config::get(file = "config/config.yml")
  
  if(class(meta) != cfg$obj.timeseries.meta.class) {
    stop(paste("Metadata not of class", cfg$obj.timeseries.meta.class))
  }
  
  if(!cfg$obj.timeseries.class %in% class(timeseries)) {
    stop(paste("Timeseries not of class", cfg$obj.timeseries.class))
  }
  
  #-----------------------------------------------------#
  ### Determine whether this timeseries already exits ###
  #-----------------------------------------------------#
  
  query <- sprintf(paste(
    "SELECT data_id FROM", 
    "series", 
    "WHERE",
    "code = %i AND type_id = %i AND element_id = %i AND aggregation = %s"), 
    meta$sta_id,
    meta$sta_type_id,
    meta$var_id,
    paste0("'", meta$var_interval, "'"))
  
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
    meta$dat_id <- db.new.data.id(db)
    
    # Create data to insert into database
    insert.data <- data.table(
      data_id = meta$dat_id,
      code = meta$sta_id, 
      type_id = meta$sta_type_id, 
      element_id = meta$var_id, 
      aggregation = paste0("'", meta$var_interval,"'"))
    
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
      print(paste("Inserted", rows.affected, "new series"))
    }
    
  } else {
    
    #------------------------------------------------------------------------#
    ### The timeseries already exist. So we delete it and insert a new one ###
    #------------------------------------------------------------------------#
    
    meta$dat_id <- result$data_id
    
    query <- sprintf(paste(
      "DELETE FROM",
      "%s_series_%s",
      "WHERE",
      "data_id = %i"
    ), 
    meta$var_interval,
    meta$var_name,
    meta$dat_id)
    
    rows.affected <- dbExecute(db, query)
    
    if(rows.affected == timeseries.length) {
      print(paste("Deleted", rows.affected, "rows from the database"))
    } else {
      warning(paste("Warning, not all records were updated:", timeseries.length, "inserted,", rows.affected, "deleted."))
    }
    
    query <- sprintf(paste(
      "DELETE FROM",
      "series_derived",
      "WHERE",
      "data_id = %i"
    ),
    meta$dat_id)
    
    rows.affected <- dbExecute(db, query)
    
    
  }
  
  #-------------------------------#
  ### Insert the new timeseries ###
  #-------------------------------#
  
  query <- sprintf(paste(
    "INSERT INTO",
    "series_derived",
    "(data_id, start, stop)",
    "VALUES",
    "(%i, %s, %s)"
  ),
  meta$dat_id,
  min(timeseries$datetime),
  max(timeseries$datetime))
  
  rows.affected <- dbExecute(db, query)
  
  timeseries$value[is.na(timeseries$value)] <- cfg$database.na.value
  timeseries <- data.table(data_id = meta$dat_id, timeseries)
  timeseries <- apply(timeseries, 1, paste, collapse = ",")
  timeseries <- paste0("(", timeseries, ")")
  timeseries <- paste(timeseries, collapse = ",")
  
  query <- sprintf(paste(
    "INSERT INTO",
    "%s_series_%s",
    "(data_id, date, value)",
    "VALUES",
    "%s"
  ),
  meta$var_interval,
  meta$var_name,
  timeseries)
  
  rows.affected <- dbExecute(db, query)
  
  print(sprintf(paste("Inserted", rows.affected, "rows into", "%s_series_%s"), meta$var_interval, meta$var_name))
  
  return(rows.affected == timeseries.length)
}

db.insert.breakdetection.results <- function(db, DB_output) {
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  query <- "DELETE FROM break_detection"
  rows.affected <- dbExecute(db, query)
  print(paste("Deleted", rows.affected, "rows from the database"))
  
  
  values <- apply(BD_output, 1, function(row){
      a <- paste(row, collapse = "','")
      a <- paste0("('", a, "')")
  })
  
  values <- paste(values, collapse = ",")
  
  query <- sprintf(paste(
    
    "INSERT INTO",
      "break_detection",
    "VALUES",
      "%s"
  ), values)
  
  rows.affected <- dbExecute(db, query)
  print(paste("Inserted", rows.affected, "rows into the database"))
  
  return(rows.affected == nrow(DB_output))
}

#' @title Fetch a new data ID from the database
#' @param db Handle to MySQL database, taken from db.setup()
#' @return A new and unique data ID
#' @description a function
#' @export
db.new.data.id <- function(db) {
  
  if(!dbIsValid(db)) {
    stop("Invalid database connection")
  }
  
  # Fetch the maximum data_id from the database
  result.ref <- dbSendQuery(db, "SELECT MAX(data_id) AS max_data_id FROM series")
  data.ID <- dbFetch(result.ref, n = 1)$max_data_id + 1
  dbClearResult(result.ref)
  return(data.ID)
}

#' @title Execute a database operation
#' @description This function is meant for quickly doing a database operation without needing to open and close a connection explicitly. This is all done automatically.
#' @param FUN One of the db.<operation> functions
#' @param ... Arguments to pass to the function
#' @return Output of the function
#' @author Jurian
#' #@example data.container <- db.execute(db.select.all, "1day", "H", "RD")
#' #@example data.id <- db.execute(db.new.data.id)
#' @export
db.execute <- function(FUN, ...) {
  
  # Set up a connection to the database
  db <- db.setup()
  
  result <- tryCatch({
    FUN(db, ...)
  }, finally = {
    # Make sure the connection is properly closed, even if there was an error
    db.close(db)
  })
  
  return(result)
}

#' @title Get station information from database
#' @description get the metadata for all stations
#' @author Marieke 
#' @export
station.info <- function(db){

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
  dbClearResult(db.q)

  return(results)
}

#' @title Get information from nearby stations from the database
#' @description get the metadata for all stations, input looks like code_real="260_H"
#' @author Marieke 
#' @param code_real code like 260_H
#' @export
#' 
station.nearby<-function(db, code_real) {
  
  split<-unlist(strsplit(code_real,"_"))
  code=split[1]
  type=split[2]
  
  query<-"SELECT * FROM nearby_stations"
  
  
  query_new<-sprintf("SELECT name,

CONCAT(nearby_stations.nearby_code,'_',types.type) as nearby_code_real,
                             latitude,
                             longitude
                      FROM nearby_stations,stations,types
                      WHERE nearby_stations.nearby_code=stations.code and
                            nearby_stations.nearby_type_id=stations.type_id and
                            nearby_stations.type_id=types.type_id and
                            nearby_stations.code=%s and types.type='%s';",
                     code,type)
  

  db.q <- dbSendQuery(db,query_new)
  results <- dbFetch(db.q, n=-1)
  dbClearResult(db.q)

  return(results)
}
