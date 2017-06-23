#' @title Aggregate 8 am to 8 am data to yearly sums
#' @description  The function takes the R object, calculates aggregates and returns this in the object, included updated meta data. 
#' @details When data availability is below 80% in a season (i.e. >18 days missing), or a year (i.e. > 73 days missing), the aggregation becomes NA.
#' @param data.container The R object containing all timeseries and metadata
# ' @param all.stations default is "TRUE" means all stations are aggregated. If all.stations = FALSE, an array of sta_ID needs to be provided.
# ' @param sta_type default is "AWS", but could be extended to for instance "WOW" in the future.
# ' @param var_id default is "RD" for daily rainfall.
# ' @param sta_id defines the string of sta_id's that need to be aggregated. Only applies when all.stations = FALSE.
# ' #@example aggregated.seasonal <- aggregate.to.seasonal(obj)
#' @author Jurian and Lotte
#' @export

aggregate2Year <- function(data.container) {
  
  if(is.null(data.container$`1day`)) {
    stop("Data container does not have daily data")
  }
  
  cfg <- config::get(file = "config/config.yml")
  # If exceeded, the day should be excluded.
  data.availability.threshold <- cfg$data.availability.threshold
  MaxNAPerYear <- floor((1 - data.availability.threshold) * 365)  
  
  agg.year <- function(timeseries) {
    timeseries$datetime <- as.Date(timeseries$datetime, format="%Y%m%d")
    years <- factor(year(timeseries$datetime))
    
    timeseries <- rbindlist(by(timeseries, years, function(y) {
      dt <- data.table (
        datetime = paste0(year((y$datetime)[1]) + 1, "01", "01", "08", "0000"),
        value = sum(y$value, na.rm = T)
      )
      setkey(dt, datetime)
      
      no.of.NAs <- sum(is.na(y$value))
      if(no.of.NAs > MaxNAPerYear) {
        dt$value <- NA
      }
      
      return(dt)
    }))
    
    class(timeseries) <- append(class(timeseries), cfg$obj.timeseries.class)
    return(timeseries)
  }

  data.container$year <- list()
  data.container$year$data <- lapply(data.container$`1day`$data, agg.year)
  data.container$year$meta <- lapply(data.container$`1day`$meta, function(m) {
    m$var_interval <- "year"
    class(m) <- cfg$obj.timeseries.meta.class
    return(m)
  })
  
  return(data.container)
}

#' @title Aggregate 8 am to 8 am data to seasonal sums
#' @description Since the variability is seasonal dependent we would like to be able to compare certain seasons only. The function takes the R object, calculates aggregates and returns this in the object, included updated meta data. 
#' @details When data availability is below 80% in a season (i.e. >18 days missing), or a year (i.e. > 73 days missing), the aggregation becomes NA.
#' @param data.container The R object containing all timeseries and metadata
# ' @param all.stations default is "TRUE" means all stations are aggregated. If all.stations = FALSE, an array of sta_ID needs to be provided.
# ' @param sta_type default is "AWS", but could be extended to for instance "WOW" in the future.
# ' @param var_id default is "RD" for daily rainfall.
# ' @param sta_id defines the string of sta_id's that need to be aggregated. Only applies when all.stations = FALSE.
# ' #@example aggregated.seasonal <- aggregate.to.seasonal(obj)
#' @author Jurian and Lotte
#' @export

aggregate2Seasonal <- function(data.container) {
  
  if(is.null(data.container$`1day`)) {
    stop("Data container does not have daily data")
  }
  
  cfg <- config::get(file = "config/config.yml")
  # If exceeded, the day should be excluded.
  data.availability.threshold <- cfg$data.availability.threshold
  MaxNAPerSeason <- floor((1 - data.availability.threshold) * (365 / 4))   

  
  # Define meteorological seasons
  winter <- c(12, 1, 2)
  spring <- c(3, 4, 5)
  summer <- c(6, 7, 8)
  autumn <- c(9, 10, 11)
  
  agg.season <- function(timeseries) {
    
    timeseries$datetime <- as.Date(timeseries$datetime, format="%Y%m%d")
    
    timeseries$months <- month(timeseries$datetime)
    timeseries$years <- year(timeseries$datetime)
    
    decembers.idx <- which(timeseries$months == 12)
    timeseries$years[decembers.idx] <- timeseries$years[decembers.idx] + 1
    
    # Assign seasons to observations
    seasons <- character(length = length(timeseries$months))
    seasons[timeseries$months %in% winter] <- "winter"
    seasons[timeseries$months %in% spring] <- "spring"
    seasons[timeseries$months %in% summer] <- "summer"
    seasons[timeseries$months %in% autumn] <- "autumn"

    seasons <- factor(paste0(seasons, timeseries$years))
    timeseries$seasons <- seasons
    
    #return(timeseries)
    
    timeseries <- rbindlist(by(timeseries, seasons, function(s) {
      
      m <- rev(s$months + 1)[1]
      m <- ifelse(nchar(m) == 1, paste0("0",m), m)
      
      dt <- data.table (
        datetime = paste0(s$years[1], m, "01", "08", "0000"),
        value = sum(s$value, na.rm = T)
      )
      setkey(dt, datetime)
      
      nr.of.months <- length(table(month(s$datetime)))
      if(nr.of.months < 3) {
        dt$value <- NA
      }
      
      nr.of.NAs <- sum(is.na(s$value))
      if(nr.of.NAs > MaxNAPerSeason) {
        dt$value <- NA
      }
      
      return(dt)
    }))
    
    setkey(timeseries, datetime)
    class(timeseries) <- append(class(timeseries), cfg$obj.timeseries.class)
    return(timeseries)
  }
  
  data.container$season <- list()
  data.container$season$data <- lapply(data.container$`1day`$data, agg.season)
  data.container$season$meta <- lapply(data.container$`1day`$meta, function(m) {
    m$var_interval <- "season"
    class(m) <- cfg$obj.timeseries.meta.class
    return(m)
  })
  
  return(data.container)
}

#' Daily aggregation
#' @title Aggregate hourly data to daily from 8am to 8am the next day
#' @description Rain is measured hourly in the automatic weather stations, this needs to be converted to sums from 8 am to 8 am on the next day.
#' @param data.container The R object containing all timeseries and metadata
# ' @param all.stations default is "TRUE" means all stations are aggregated. If all.stations = FALSE, an array of sta_ID needs to be provided.
# ' @param sta_type default is "AWS", but could be extended to for instance "WOW" in the future.
# ' @param var_id default is "RH" for hourly rainfall.
# ' @param sta_id defines the string of sta_id's that need to be aggregated. Only applies when all.stations = FALSE.
# ' #@example aggregated88 <- aggregate.to.88(obj=obj)
#' @author Lotte, Jurian & Hidde
#' @export

aggregate288 <- function(data.container) {
  
  if(is.null(data.container$`1hour`)) {
    stop("Data container does not have hourly data")
  }
  
  cfg <- config::get(file = "config/config.yml")
  data.availability.threshold <- cfg$data.availability.threshold
  # If exceeded, the day should be excluded.
  MaxNAPerDay <- floor((1 - data.availability.threshold) * 24)   
  
  agg.88 <- function(timeseries) {
    
    # Make timeline of timestamps 0800 indicating the end of each day
    # Use integers for really fast comparison
    first_timestep <- which(hour(strptime(timeseries$datetime, format="%Y%m%d%H%M%S")) == 9)[1]
    # Last occurence of 8, same as first occurance of the reverse
    last_timestep <- rev(which(hour(strptime(timeseries$datetime, format="%Y%m%d%H%M%S")) == 8))[1]
    
    if(is.na(first_timestep) | is.na(last_timestep)) stop("Incomplete timeperiod")
    
    # Aggregate rainfall in the 24 hours belonging to the 0800-0800 timeframe      
    nrdays <- length(first_timestep:last_timestep) / 24
    
    if(round(nrdays) != nrdays) stop("Incomplete timeperiod")
   
    time_agg <- rep(1:nrdays, each = 24 )
    timeselec <- timeseries$value[first_timestep:last_timestep]  
    
    # If more than 4 hours per 24 hours is NA, the dayvalue should be NA (>80% data availability rule)
    days_with_NAvalues <- time_agg[which(is.na(timeselec))] 
    NAvaluestable <- as.data.frame(table(days_with_NAvalues))
    days_with_over20percent_NAvalues <- as.numeric(as.character( NAvaluestable[which(NAvaluestable$Freq>MaxNAPerDay),1] ))
    
    value_agg <- setDT(as.data.table(timeselec))[,lapply(.SD,sum, na.rm=T),by=.(time_agg)]$timeselec
    value_agg[days_with_over20percent_NAvalues] <- NA
    
    timeseries <- timeseries[seq((first_timestep + 23 ), last_timestep, by = 24), 1]
    timeseries$value <- value_agg
    
    class(timeseries) <- append(class(timeseries), cfg$obj.timeseries.class)
    return(timeseries)
  }
#  count <- 0
  data.container$`1day` <- list()
  data.container$`1day`$data <- lapply(data.container$`1hour`$data, agg.88)
  data.container$`1day`$meta <- lapply(data.container$`1hour`$meta, function(m) {
    m$var_interval <- "1day"
    class(m) <- cfg$obj.timeseries.meta.class
    return(m)
  })

  return(data.container)
}
