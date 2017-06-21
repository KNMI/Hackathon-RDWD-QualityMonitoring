library(shiny)
#runApp("~/Hackathon-RDWD-QualityMonitoring/inst/shiny")
library(leaflet)
library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)

# stations <-readRDS("~/Hackathon-RDWD-QualityMonitoring/data/testdata/stationInfo.rds")
# stations.nearby <-readRDS("~/Hackathon-RDWD-QualityMonitoring/data/testdata/stationNearby.rds")

#query from the db
stations<-station.info()
# stations.nearby<-station.nearby()

#Create a spatialpointsdataframe to calculate distances later in the server
spdf<-stations
coordinates(spdf)<-~longitude+latitude
proj4string(spdf)<-CRS("+init=epsg:4326")


server <- function(input, output, session) {
  detection_datetime <- Sys.time()
  
  rv <- reactiveValues(showDetails = "false", station = "")
  data <- reactiveValues(clickedMarker = NULL)
  
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
    data$clickedMarker <- markerClickEvent
    
    df<-reactive({
    sp<-data.frame(data$clickedMarker)
    coordinates(sp)<-~lng+lat
    proj4string(sp)<-CRS("+init=epsg:4326")

    d<-pointDistance(sp,spdf,lonlat=TRUE)
    d<-d/1000
    df<-data.table(stations,d)
    setkey(df,"d")
    df<-df[which(df$type_id==input$Type),]
    df.radius<-df[which(df$d<input$Radius),]
    })
    
    dfNr<-reactive({
      sp<-data.frame(data$clickedMarker)
      coordinates(sp)<-~lng+lat
      proj4string(sp)<-CRS("+init=epsg:4326")
      
      d<-pointDistance(sp,spdf,lonlat=TRUE)
      d<-d/1000
      df<-data.table(stations,d)
      setkey(df,"d")
      df<-df[which(df$type_id==input$Type),]
      df.number<-head(df,n=input$nr)
    })
    
    #NOT WORKING!!!
    dfNearby<-reactive({
      data$clickedMarker <- markerClickEvent
      dfNearby<-station.nearby(data$clickedMarker$code,data$clikedMarker$id) #function making a connection to the db
      # if (!is.null(dfNearby)){paste("The station you selected is not on the list")}
      # dfNearby<-unique(dfNearby$nearby_code)
      dfNearby
    })
    output$stationsNearby<-renderTable({
      # if (!is.null(dfNearby())){paste("The station you selected is not on the list")}
      dfNearby()
    })
    ##############
    
    output$clickedMarker <- renderText({
      paste("Station ", data$clickedMarker$id, "has been selected")
    })
    
    
    print(data$clickedMarker)
    
    output$clickedDistance <- renderTable({
      df()
    })
    
    output$clickedNumber <- renderTable({
      dfNr()
    })
    
    
    
  }
  
  mapClicked <- function(mapClickEvent) {
    print("observed map click")
    data$clickedMarker <- NULL
    print(data$clickedMarker)
    output$clickedMarker <-
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
  
  output$plot <- renderPlot({
    if (input$go == 0)
      return()
    
    isolate(p)
  })
  
  
  pal <- colorFactor(c("green", "orange"),domain = c("1","2"))
  
  output$map <- renderLeaflet(
    leaflet(stations) %>%
      setView(lng=5.3878, lat=52.1561, zoom=7) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lat = ~ latitude,
        lng = ~ longitude,
        popup = ~ name,
        layerId = ~ code,
        color = ~pal(type_id)
      )
  )
  
  output$clickedMarker <- renderText("Please select a station")
  
  outputOptions(output, "showDetails", suspendWhenHidden = FALSE)
  outputOptions(output, "stationId", suspendWhenHidden = FALSE)
  
}