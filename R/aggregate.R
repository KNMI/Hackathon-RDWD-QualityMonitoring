library(data.table)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "test")

#' Load test data
#' 
#' @title Load in test data from Mariekes directory
#' @description This is just a temporary function to make testing of aggregation possible. 
#' @author Jurian and Lotte


grab.test.data <- function() {
  
  cfg <- config::get(file = "config/config.yml")
  
  marieke.dir <- cfg$test.data.dir
  
  data.names <- lapply(list.files(marieke.dir), function(x) {
    name <- substr(x, 0, nchar(x) - 4)
  })
  data.tables <- lapply(list.files(marieke.dir), function(x) {
    fread(paste0(marieke.dir, x))  
  })
  names(data.tables) <- data.names
  
  return(data.tables)
}


#' Calculate seasonal sums
#' @title Aggregate 8am to 8am data to yearly sums and seasonal sums
#' @description Since the variability is seasonal dependent we would like to be able to compare certain seasons only.
#' @details When data availability is below 80% in a season (i.e. >18 days missing), or a year (i.e. > 73 days missing), the aggregation becomes -9999.
#' @param aggregate88 The 8-8 aggregated data to be further aggregated to seasonal
#' #@example aggregated.seasonal <- aggregate.to.seasonal(data.tables$`DeBilt_550_N_8-8daily_precip`)
#' @author Jurian



aggregate.to.seasonal <- function(aggregate88) {
  
  cfg <- config::get(file = "config/config.yml")
  data.availability.threshold <- cfg$data.availability.threshold
  MaxNAPerSeason <- floor((1-data.availability.threshold)*(365/4))  #if exceeded, the day should be excluded. 
  MaxNAPerYear <- floor((1-data.availability.threshold)*(365))  #if exceeded, the day should be excluded. 
  
  # Define meteorological seasons
  winter <- c(12, 1, 2)
  spring <- c(3, 4, 5)
  summer <- c(6, 7, 8)
  autumn <- c(9, 10, 11)
  
  names(aggregate88) <- c("date", "value")
  # Convert the character vector to a date so we can perform operations like month() and year()
  aggregate88$date <- as.Date(as.character(aggregate88$date), format="%Y%m%d")
  aggregate88$value[which(aggregate88$value == -9999)] <- NA
    
  # Collect months
  aggregate88$month <- month(aggregate88$date)
  # Collect years
  aggregate88$year <- year(aggregate88$date)
  
  decembers.idx <- which(aggregate88$month == 12)
  
  aggregate88$year[decembers.idx] <- aggregate88$year[decembers.idx] + 1
  
  # Assign seasons to observations
  aggregate88$season <- 0
  aggregate88$season[aggregate88$month %in% winter] <- "winter"
  aggregate88$season[aggregate88$month %in% spring] <- "spring"
  aggregate88$season[aggregate88$month %in% summer] <- "summer"
  aggregate88$season[aggregate88$month %in% autumn] <- "autumn"
  # Make sure the columns are in the correct ordering
  aggregate88$season <- factor(aggregate88$season, levels = c("winter", "spring", "summer", "autumn"))

  # Find the locations where value is NA
  year_season_NAvalues <- aggregate88[which(is.na(aggregate88$value)),c(4,5)]
  NAvaluestable <- as.data.frame(table(year_season_NAvalues))
  # A season has 365/4 = 91.25 days. 80% data availability corresponds with no more than 18 days with NA values. 
  seasons_with_over20percent_NAvalues <- NAvaluestable[which(NAvaluestable$Freq>MaxNAPerSeason),c(1,2)]
  year_NAvalues <- aggregate88$year[which(is.na(aggregate88$value))]
  NAvaluestable <- as.data.frame(table(year_NAvalues))
  # 80% data availability corresponds with no more than 73 days with NA values. 
  years_with_over20percent_NAvalues <- as.character(NAvaluestable[which(NAvaluestable$Freq>MaxNAPerYear),1])
    
  # Create a data.table for calendar yearly sums
  yearly.sums <- data.table(aggregate(list(y = aggregate88$value), by = list(year = year(aggregate88$date)), sum, na.rm=T))
  # Set key for really fast merging later on
  setkey(yearly.sums, year)
  
  # Create a list of data.tables, one for each season, this makes it easier to merge later on than
  # aggregating by year AND season at once. This is because we need them merged column wise and not row wise.
  aggregate.sums <- by(aggregate88, aggregate88$season, function(x) {
    seasonal.sum <- data.table(aggregate(list(value = x$value), list(year = x$year), sum, na.rm=T))
    # Set key for really fast merging later on
    setkey(seasonal.sum, year)
    return(seasonal.sum)
  })
  
  # Merge all data tables in the list
  aggregate.seasonal <- Reduce(function(a, b) merge(a, b, by = "year", all = TRUE), aggregate.sums)
  # Fix names so we can merge the yearly sums
  names(aggregate.seasonal) <- c("year", "djf", "mam", "jja", "son")
  # Merge the yearly sums with the seasonal sums
  aggregate.seasonal <- merge(yearly.sums, aggregate.seasonal, by = "year")
  
  # Insert NA for output that has been constructed on less than 80% data availability. 
  aggregate.seasonal$y[which(aggregate.seasonal$year %in% years_with_over20percent_NAvalues)] <- -9999
  aggregate.seasonal$djf[which(aggregate.seasonal$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="winter"])] <- -9999
  aggregate.seasonal$mam[which(aggregate.seasonal$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="spring"])] <- -9999
  aggregate.seasonal$jja[which(aggregate.seasonal$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="summer"])] <- -9999
  aggregate.seasonal$son[which(aggregate.seasonal$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="autumn"])] <- -9999
    
  # Profit!
  return(aggregate.seasonal)
}


#' Daily aggeration
#' @title Aggregate hourly data to daily from 8am to 8am the next day
#' @description Rain is measured hourly in the automatic weather stations, this needs to be converted to sums from 8 am to 8 am on the next day.
#' @param obj The R object containing all timeseries and metadata
#' @param all.stations default is "TRUE" means all stations are aggregated. If all.stations = FALSE, an array of sta_ID needs to be provided. 
#' @param sta_type default is "AWS", but could be extended to for instance "WOW" in the future.
#' @param var_id default is "RH" for hourly rainfall. 
#' @param sta_id defines the subset of sta_ID's that need to be aggregated. Only applies when all.stations = FALSE. 
#' @example aggregated88 <- aggregate.to.88(obj=obj)
#' @author Lotte

#source("R/dbOperations.R")
#db <- setup.db()
#obj <- query.hourly(db)

#function makes aggregations from all AWS datasets. 
#default is all.stations=TRUE. if all.stations=FALSE, specify sta_ID. 
aggregate.to.88 <- function(obj, all.stations=TRUE, sta_type="AWS", var_ID="RH", sta_id=NULL){
    cfg <- config::get(file = "config/config.yml")
    data.availability.threshold <- cfg$data.availability.threshold
    MaxNAPerDay <- floor((1-data.availability.threshold)*24)  #if exceeded, the day should be excluded. 
  
  if(all.stations==FALSE){
    if(is.null(sta_ID)){stop("At least one sta_ID needs to be provided")}

    seriesidselec <- sapply(obj$meta,function(m){if(m$sta_type=="AWS" & m$var_id =="RH" & m$sta_id %in% sta_id){
      return(TRUE)
        }else{
          return(FALSE)}})
    
    }else{ # in case of default: all.stations=TRUE
      
   seriesidselec <- sapply(obj$meta,function(m){if(m$sta_type=="AWS" & m$var_id =="RH"){
     return(TRUE)
        }else{
          return(FALSE)}})
    }

  seriesidlist <- names(obj$meta)[seriesidselec]
  
  for(sid in 1:length(names(obj$hourly))){
    if(names(obj$hourly)[[sid]] %in% seriesidlist){

  hourly <- obj$hourly[[sid]]

  hourly$value[which(hourly$value==-9999)] <- NA 
  
  # Make timeline of timestamps 0800 indicating the end of each day
  # Use integers for really fast comparison
  first_timestep <- which(hour(strptime(hourly$datetime, format="%Y%m%d%H%M%S")) == 9)[1]
  # Last occurence of 8, same as first occurance of the reverse
  last_timestep <- rev(which(hour(strptime(hourly$datetime, format="%Y%m%d%H%M%S")) == 8))[1]
  
  # aggregate rainfall in the 24 hours belonging to the 0800-0800 timeframe          
  nrdays <- length(first_timestep:last_timestep) / 24
  time_agg <- rep(1:nrdays, each = 24 )
  timeselec <- hourly$value[first_timestep:last_timestep]  

  days_with_NAvalues <- time_agg[which(is.na(timeselec))] # if more than 4 hours per 24 hours is NA, the dayvalue should be NA (>80% data availability rule)
  NAvaluestable <- as.data.frame(table(days_with_NAvalues))
  days_with_over20percent_NAvalues <- as.numeric(as.character( NAvaluestable[which(NAvaluestable$Freq>MaxNAPerDay),1] ))
  
  value_agg <- setDT(as.data.frame(timeselec))[,lapply(.SD,sum, na.rm=T),by=.(time_agg)]$timeselec
  value_agg[days_with_over20percent_NAvalues] <- -9999
  
  # output file
  aggregated_data <- hourly[seq((first_timestep + 23 ), last_timestep, by = 24), 1]
  aggregated_data$value <- value_agg
  
  newseriesid <- as.character(as.numeric(names(obj$hourly)[[sid]]) + 100000000)
  obj$meta[newseriesid]
  
    } #end if-loop
  } #end for-loop
  
  return(obj)
}







