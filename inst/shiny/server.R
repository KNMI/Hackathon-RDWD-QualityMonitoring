library(shiny)
#runApp("~/Hackathon-RDWD-QualityMonitoring/inst/shiny")

library(leaflet)

library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)


station.info <- function() {
  db <- db.setup()
  query <- "SELECT * FROM stations"
  
  db <- dbSendQuery(db, query)
  results <- dbFetch(db)
  
  
  dbClearResult(db)
  print(results)
  return(results)
  
}

stations <-
  readRDS("~/Hackathon-RDWD-QualityMonitoring/data/testdata/stationInfo.rds")

#datafiles<-list.files("/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata/",pattern="daily",full.names = TRUE)
# datafiles<-list.files("/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata/",pattern="hourly",full.names = TRUE)

testfile <-
  "/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata//DeBilt_260_H_hourly_precip.csv"
Bilt <- fread(testfile)
names(Bilt) <- c("day", "hour", "P")


Bilt$hour <- str_pad(as.integer(Bilt$hour), width = 6, pad = "0")
Bilt$time <- paste0(Bilt$day, " ", Bilt$hour)
Bilt$time <- as.POSIXct(Bilt$time, format = "%Y%m%d %H%M%S")
# Bilt$day<-as.Date(Bilt$day,format="%Y%m%d")

p <- ggplot(Bilt, aes(time, P)) +
  geom_point()

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
    output$clickedMarker <- renderText({
      paste("Station ", data$clickedMarker$id, "has been selected")
    })
    print(data$clickedMarker)
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
  
  
  
  output$map <- renderLeaflet(
    leaflet(stations) %>%
      setView(lng=5.3878, lat=52.1561, zoom=7) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(
        lat = ~ latitude,
        lng = ~ longitude,
        popup = ~ name,
        layerId = ~ code
      )
  )
  
  output$clickedMarker <- renderText("Please select a station")
  
  outputOptions(output, "showDetails", suspendWhenHidden = FALSE)
  outputOptions(output, "stationId", suspendWhenHidden = FALSE)
  
}