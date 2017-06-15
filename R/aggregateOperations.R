library(data.table)
library(config)

#' Calculate seasonal sums
#' @title Aggregate 8am to 8am data to yearly sums and seasonal sums
#' @description Since the variability is seasonal dependent we would like to be able to compare certain seasons only. The function takes the R object, calculates aggregates and returns this in the object, included updated meta data. 
#' @details When data availability is below 80% in a season (i.e. >18 days missing), or a year (i.e. > 73 days missing), the aggregation becomes -9999.
#' @param obj The R object containing all timeseries and metadata
#' @param all.stations default is "TRUE" means all stations are aggregated. If all.stations = FALSE, an array of sta_ID needs to be provided. 
#' @param sta_type default is "AWS", but could be extended to for instance "WOW" in the future.
#' @param var_id default is "RD" for daily rainfall. 
#' @param sta_id defines the string of sta_id's that need to be aggregated. Only applies when all.stations = FALSE. 
#' @example aggregated.seasonal <- aggregate.to.seasonal(obj)
#' @author Jurian and Lotte

aggregate.to.seasonal <- function(obj, all.stations=TRUE, sta_type="AWS", var_id="RD", sta_id=NULL) {
  cfg <- config::get(file = "config/config.yml")
  data.availability.threshold <- cfg$data.availability.threshold
  MaxNAPerSeason <- floor((1-data.availability.threshold)*(365/4))  #if exceeded, the day should be excluded. 
  MaxNAPerYear <- floor((1-data.availability.threshold)*(365))  #if exceeded, the day should be excluded. 
  
  if(all.stations){
    
    seriesidselec <- sapply(obj$meta,function(m){
      m$sta_type==sta_type & m$var_id == var_id
    })
    
  }else{ # in case of default: all.stations=TRUE
    
    if(is.null(sta_id)){stop("At least one sta_id needs to be provided")}
    
    seriesidselec <- sapply(obj$meta,function(m){
      m$sta_type==sta_type & m$var_id == var_id & m$sta_id %in% sta_id
    })

  }
  
  seriesidlist <- names(obj$meta)[seriesidselec]
  
  # Define meteorological seasons
  winter <- c(12, 1, 2)
  spring <- c(3, 4, 5)
  summer <- c(6, 7, 8)
  autumn <- c(9, 10, 11)
  
  for(sid in 1:length(names(obj$daily))){
    if(names(obj$daily)[[sid]] %in% seriesidlist){
      
      daily <- obj$daily[[sid]]
      daily$datetime <- as.Date(as.character(daily$datetime), format="%Y%m%d")

      # Collect months
      daily$month <- month(daily$datetime)
      # Collect years
      daily$year <- year(daily$datetime)
      
      decembers.idx <- which(daily$month == 12)
      
      daily$year[decembers.idx] <- daily$year[decembers.idx] + 1
      
      # Assign seasons to observations
      daily$season <- 0
      daily$season[daily$month %in% winter] <- "winter"
      daily$season[daily$month %in% spring] <- "spring"
      daily$season[daily$month %in% summer] <- "summer"
      daily$season[daily$month %in% autumn] <- "autumn"
      # Make sure the columns are in the correct ordering
      daily$season <- factor(daily$season, levels = c("winter", "spring", "summer", "autumn"))
      
      # Find the locations where value is NA
      year_season_NAvalues <- daily[which(is.na(daily$value)),c(4,5)]
      NAvaluestable <- as.data.frame(table(year_season_NAvalues))
      # A season has 365/4 = 91.25 days. 80% data availability corresponds with no more than 18 days with NA values. 
      seasons_with_over20percent_NAvalues <- NAvaluestable[which(NAvaluestable$Freq>MaxNAPerSeason),c(1,2)]
      year_NAvalues <- daily$year[which(is.na(daily$value))]
      NAvaluestable <- as.data.frame(table(year_NAvalues))
      # 80% data availability corresponds with no more than 73 days with NA values. 
      years_with_over20percent_NAvalues <- as.character(NAvaluestable[which(NAvaluestable$Freq>MaxNAPerYear),1])

      # Create a data.table for calendar yearly sums
      yearly.sums <- data.table(aggregate(list(y = daily$value), by = list(year = year(daily$date)), sum, na.rm=T))
      # Set key for really fast merging later on
      setkey(yearly.sums, year)      
      
      # Create a list of data.tables, one for each season, this makes it easier to merge later on than
      # aggregating by year AND season at once. This is because we need them merged column wise and not row wise.
      aggregate.sums <- by(daily, daily$season, function(x) {
        seasonal.sum <- data.table(aggregate(list(value = x$value), list(year = x$year), sum, na.rm=T))
        # Set key for really fast merging later on
        setkey(seasonal.sum, year)
        return(seasonal.sum)
      })
      
      # Insert NA for output that has been constructed on less than the data availability threshold. 
      yearly.sums$y[which(yearly.sums$year %in% years_with_over20percent_NAvalues)] <- NA
      aggregate.sums$winter$value[which(aggregate.sums$winter$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="winter"])] <- NA
      aggregate.sums$spring$value[which(aggregate.sums$spring$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="spring"])] <- NA
      aggregate.sums$summer$value[which(aggregate.sums$summer$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="summer"])] <- NA
      aggregate.sums$autumn$value[which(aggregate.sums$autumn$year %in% seasons_with_over20percent_NAvalues$year[seasons_with_over20percent_NAvalues$season=="autumn"])] <- NA
      
      # Add new yearly file to obj. 
      newseriesid <- as.character(as.numeric(names(obj$daily)[[sid]]) - cfg$obj.base.id.daily + cfg$obj.base.id.yearly) #annual series id's have precursor 4, daily serie id's have precursor 2.

      dt <- as.data.table(yearly.sums)
      names(dt) <- c("datetime", "value")
      obj$yearly$y <- c(obj$yearly$y, list(dt))
      n <- names(obj$yearly$y)
      names(obj$yearly$y) <- c( n[-length(n)], newseriesid)
      
      write.meta.yearly.dt <- function(obj, dt, sid, newseriesid){   
        meta <- obj$meta[[which(names(obj$meta) ==  names(obj$daily)[[sid]])]]
        meta$var_period <- "year"
        meta$var_id <- "RA"
        meta$ser_current <- dt$datetime[nrow(dt)] #last timestamp of daily timeseries. 
        obj$meta <- c(obj$meta, list(meta))
        n <- names(obj$meta)
        names(obj$meta) <- c( n[-length(n)], newseriesid)
        return(obj)
      }
      obj <- write.meta.yearly.dt(obj=obj, dt=dt, sid=sid, newseriesid=newseriesid)
      
      # djf
      newseriesid <- as.character(as.numeric(names(obj$daily)[[sid]]) - cfg$obj.base.id.daily + cfg$obj.base.id.djf) #djf series id's have precursor 5
      
      dt <- as.data.table(aggregate.sums$winter)
      names(dt) <- c("datetime", "value")
      obj$yearly$djf <- c(obj$yearly$djf, list(dt))
      n <- names(obj$yearly$djf)
      names(obj$yearly$djf) <- c( n[-length(n)], newseriesid)

      obj <- write.meta.yearly.dt(obj=obj, dt=dt, sid=sid, newseriesid=newseriesid)
      
      # mam
      newseriesid <- as.character(as.numeric(names(obj$daily)[[sid]]) - cfg$obj.base.id.daily + cfg$obj.base.id.mam) #mam series id's have precursor 6
      
      dt <- as.data.table(aggregate.sums$spring)
      names(dt) <- c("datetime", "value")
      obj$yearly$mam <- c(obj$yearly$mam, list(dt))
      n <- names(obj$yearly$mam)
      names(obj$yearly$mam) <- c( n[-length(n)], newseriesid)
      
      obj <- write.meta.yearly.dt(obj=obj, dt=dt, sid=sid, newseriesid=newseriesid)
      
      # jja
      newseriesid <- as.character(as.numeric(names(obj$daily)[[sid]]) - cfg$obj.base.id.daily + cfg$obj.base.id.jja) #jja series id's have precursor 7
      
      dt <- as.data.table(aggregate.sums$summer)
      names(dt) <- c("datetime", "value")
      obj$yearly$jja <- c(obj$yearly$jja, list(dt))
      n <- names(obj$yearly$jja)
      names(obj$yearly$jja) <- c( n[-length(n)], newseriesid)
      
      obj <- write.meta.yearly.dt(obj=obj, dt=dt, sid=sid, newseriesid=newseriesid)
      
      #son
      newseriesid <- as.character(as.numeric(names(obj$daily)[[sid]]) - cfg$obj.base.id.daily + cfg$obj.base.id.son) #son series id's have precursor 8
      
      dt <- as.data.table(aggregate.sums$autumn)
      names(dt) <- c("datetime", "value")
      obj$yearly$son <- c(obj$yearly$son, list(dt))
      n <- names(obj$yearly$son)
      names(obj$yearly$son) <- c( n[-length(n)], newseriesid)
      
      obj <- write.meta.yearly.dt(obj=obj, dt=dt, sid=sid, newseriesid=newseriesid)
            
    } #end if-loop
  } #end for-loop
  
  return(obj)
}



#' Daily aggeration
#' @title Aggregate hourly data to daily from 8am to 8am the next day
#' @description Rain is measured hourly in the automatic weather stations, this needs to be converted to sums from 8 am to 8 am on the next day.
#' @param obj The R object containing all timeseries and metadata
#' @param all.stations default is "TRUE" means all stations are aggregated. If all.stations = FALSE, an array of sta_ID needs to be provided. 
#' @param sta_type default is "AWS", but could be extended to for instance "WOW" in the future.
#' @param var_id default is "RH" for hourly rainfall. 
#' @param sta_id defines the string of sta_id's that need to be aggregated. Only applies when all.stations = FALSE. 
#' @example aggregated88 <- aggregate.to.88(obj=obj)
#' @author Lotte, Jurian & Hidde

#function makes aggregations from all AWS datasets. 
#default is all.stations=TRUE. if all.stations=FALSE, specify sta_ID. 
aggregate.to.88 <- function(obj, all.stations=TRUE, sta_type="AWS", var_id="RH", sta_id=NULL){
    cfg <- config::get(file = "config/config.yml")
    data.availability.threshold <- cfg$data.availability.threshold
    MaxNAPerDay <- floor((1-data.availability.threshold)*24)  #if exceeded, the day should be excluded. 
  
  if(all.stations){
    
    seriesidselec <- sapply(obj$meta,function(m){
      m$sta_type==sta_type & m$var_id == var_id
    })
    
    }else{ # in case of default: all.stations=TRUE
      
      if(is.null(sta_id)) stop("At least one sta_ID needs to be provided")
      
      seriesidselec <- sapply(obj$meta,function(m){
        m$sta_type==sta_type & m$var_id == var_id & m$sta_id %in% sta_id
      }) 
    }

  seriesidlist <- names(obj$meta)[seriesidselec]
  
  for(sid in 1:length(names(obj$hourly))){
    if(names(obj$hourly)[[sid]] %in% seriesidlist){

  hourly <- obj$hourly[[sid]]

  # Make timeline of timestamps 0800 indicating the end of each day
  # Use integers for really fast comparison
  first_timestep <- which(hour(strptime(hourly$datetime, format="%Y%m%d%H%M%S")) == 9)[1]
  # Last occurence of 8, same as first occurance of the reverse
  last_timestep <- rev(which(hour(strptime(hourly$datetime, format="%Y%m%d%H%M%S")) == 8))[1]
  if(is.na(first_timestep) | is.na(last_timestep))stop("Incomplete timeperiod")
  
  # Aggregate rainfall in the 24 hours belonging to the 0800-0800 timeframe          
  nrdays <- length(first_timestep:last_timestep) / 24
#  if(round(nrdays) != nrdays){stop("Incomplete timeperiod")}
  if(round(nrdays) != nrdays){warning("Incomplete timeperiod")
    return(FALSE)}
  
  time_agg <- rep(1:nrdays, each = 24 )
  timeselec <- hourly$value[first_timestep:last_timestep]  

  days_with_NAvalues <- time_agg[which(is.na(timeselec))] # if more than 4 hours per 24 hours is NA, the dayvalue should be NA (>80% data availability rule)
  NAvaluestable <- as.data.frame(table(days_with_NAvalues))
  days_with_over20percent_NAvalues <- as.numeric(as.character( NAvaluestable[which(NAvaluestable$Freq>MaxNAPerDay),1] ))
  
  value_agg <- setDT(as.data.frame(timeselec))[,lapply(.SD,sum, na.rm=T),by=.(time_agg)]$timeselec
  value_agg[days_with_over20percent_NAvalues] <- NA
  
  aggregated_data <- hourly[seq((first_timestep + 23 ), last_timestep, by = 24), 1]
  aggregated_data$value <- value_agg
  
  newseriesid <- as.character(as.numeric(names(obj$hourly)[[sid]]) - cfg$obj.base.id.hourly + cfg$obj.base.id.daily)

  # Add new daily file to obj. 
  dt <- as.data.table(aggregated_data)
  obj$daily <- c(obj$daily, list(dt))
  n <- names(obj$daily)
  names(obj$daily) <- c( n[-length(n)], newseriesid)
  
  # Add new information to meta in obj.
  
  meta <- obj$meta[[which(names(obj$meta) ==  names(obj$hourly)[[sid]])]]
  meta$var_period <- "day"
  meta$var_id <- "RD"
  meta$ser_current <- dt$datetime[nrow(dt)] #last timestamp of daily timeseries. 
  obj$meta <- c(obj$meta, list(meta))
  n <- names(obj$meta)
  names(obj$meta) <- c( n[-length(n)], newseriesid)
  
    } #end if-loop
  } #end for-loop
  
  return(obj)
}







