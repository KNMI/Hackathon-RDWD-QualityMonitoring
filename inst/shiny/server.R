#runApp("~/Hackathon-RDWD-QualityMonitoring/inst/shiny")
library(shiny)
library(leaflet)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)
# library(raster)
source("~/Hackathon-RDWD-QualityMonitoring/R/databaseOperations.R",local=TRUE)

# stations <-readRDS("~/Hackathon-RDWD-QualityMonitoring/data/testdata/stationInfo.rds")
# stations.nearby <-readRDS("~/Hackathon-RDWD-QualityMonitoring/data/testdata/stationNearby.rds")

# change name of DB_output file if required. DB's are constructed in runScripts.R
DB_output <- read.table("~/Hackathon-RDWD-QualityMonitoring/output/text/BD_output_NL_AWSvsMAN.txt", sep=",", header=T)  


#query from the db
stations.all<-station.info()
stations.all<-subset(stations.all,select=c("name",
                             "latitude",
                             "longitude",
                             "code_real",
                             "type_id",
                             "element",
                             "start",
                             "stop" ))
# stations.nearby<-station.nearby()

#Create a spatialpointsdataframe to calculate distances later in the server



server <- function(input, output, session) {
  
  stations<-reactive({stations.all[which(stations.all$start<=input$dateRange[[1]] & 
                                           stations.all$stop>=input$dateRange[[2]] & 
                                           stations.all$type_id==input$Type),]})
  detection_datetime <- Sys.time()
  
  rv <- reactiveValues(showDetails = "false", station = "")
  data <- reactiveValues(clickedStation = NULL)
  
  
  setDetails <- function(detailVisible, station) {
    if (detailVisible) {
      rv$showDetails <- "true"
      rv$station <- station
    } else {
      rv$showDetails <- "false"
      rv$station <- ""
    }
  }

  markerClicked <- function(markerClickEvent) {
    print("observed marker click")
    data$clickedStation <- markerClickEvent
  
    df<-reactive({
      spdf<-stations()
      coordinates(spdf)<-~longitude+latitude
      proj4string(spdf)<-CRS("+init=epsg:4326")  
      
    sp<-data.frame(data$clickedStation)
    coordinates(sp)<-~lng+lat
    proj4string(sp)<-CRS("+init=epsg:4326")

    d<-pointDistance(sp,spdf,lonlat=TRUE)
    d<-d/1000
    df<-data.table(stations(),d)
    setkey(df,"d")
    df<-df[which(df$type_id==input$Type),]
    df.radius<-df[which(df$d<input$Radius),]
    })
    
    dfNr<-reactive({
      spdf<-stations()
      coordinates(spdf)<-~longitude+latitude
      proj4string(spdf)<-CRS("+init=epsg:4326")
      sp<-data.frame(data$clickedStation)
   
      coordinates(sp)<-~lng+lat
      proj4string(sp)<-CRS("+init=epsg:4326")
      
      d<-pointDistance(sp,spdf,lonlat=TRUE)
      d<-d/1000
      df<-data.table(stations(),d)
      setkey(df,"d")
      df<-df[which(df$type_id==input$Type),]
      df.number<-head(df,n=input$nr+1)
    })
    

    
    #NOT WORKING!!!
    dfNearby<-reactive({
      data$clickedStation <- markerClickEvent
      dfNearby<-station.nearby(data$clickedStation$id) #function making a connection to the db
    })
    output$stationsNearby<-renderTable({
      # if (!is.null(dfNearby())){paste("The station you selected is not on the list")}
      dfNearby()
    })
    ##############
    output$clickedStation <- renderText({
      paste("Station ", data$clickedStation$id, "has been selected")
    })
    output$clickedDistance <- renderTable({
      df()
    })
    output$clickedNumber <- renderTable({
      dfNr()
    })
    print(data$clickedStation)
  }
  
  mapClicked <- function(mapClickEvent) {
    print("observed map click")
    data$clickedStation <- NULL
    print(data$clickedStation)
    output$clickedStation <-
      renderText("No station has been selected")
  }
  
  observeEvent(input$showDetailsNL, setDetails(TRUE, "NL"))
  observeEvent(input$showDetailsNLa, setDetails(TRUE, "NL"))
  observeEvent(input$showDetails260a, setDetails(TRUE, "260"))
  observeEvent(input$showDetails280, setDetails(TRUE, "280"))
  observeEvent(input$showDetails280a, setDetails(TRUE, "280"))
  observeEvent(input$hideDetails, setDetails(FALSE))
  
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$map_marker_click,
               markerClicked(input$map_marker_click))
  observeEvent(input$map_click, mapClicked(input$map_click))
  
  #output$selectedMarker <- renderText("No marker selected (initial)")
  output$showDetails <- renderText(rv$showDetails)
  output$stationId <- renderText({
    paste("Details for station", rv$station)
  })
  
  output$datetime <- renderText({
    paste(format(detection_datetime), ": NL - Annual")
  })
  
  output$datetime2 <- renderText({
    paste(format(detection_datetime - 363421), ": 280 - MAM")
  })
  
  pal <- colorFactor(c("#bcdff1", "#d0e9c6"),domain = c("1","2"))
  
  #Leaflet update not always correct...stations() not always updated 
  #This could be a solution: https://www.r-bloggers.com/r-shiny-leaflet-using-observers/
  output$map <- renderLeaflet(
    leaflet(data=stations(), options = leafletOptions(minZoom = 7, maxZoom = 13)) %>%
      setView(lng=5.3878, lat=52.1561, zoom=7) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))  %>%
      addCircleMarkers(
        lat = ~ latitude,
        lng = ~ longitude,
        popup = ~ name,
        layerId = ~ code_real,
        color = ~pal(type_id),
        stroke = FALSE, fillOpacity = 0.9,
        radius = 10)
      )
  
  observe({
    input$dateRange
    leafletProxy("map", data = stations()) %>%
      clearShapes() %>%
      addCircleMarkers(
        lat = ~ latitude,
        lng = ~ longitude,
        popup = ~ name,
        layerId = ~ code_real,
        color = ~pal(type_id),
        stroke = FALSE, fillOpacity = 0.9,
        radius = 10
      )
  })
  
  observe({
    input$Type
    leafletProxy("map", data = stations()) %>%
      clearShapes() %>%
      addCircleMarkers(
        lat = ~ latitude,
        lng = ~ longitude,
        popup = ~ name,
        layerId = ~ code_real,
        color = ~pal(type_id),
        stroke = FALSE, fillOpacity = 0.9,
        radius = 10
      )
  })
  
  output$clickedStation <- renderText("Please select a station on the map")
  outputOptions(output, "showDetails", suspendWhenHidden = FALSE)
  outputOptions(output, "stationId", suspendWhenHidden = FALSE)
  
}
