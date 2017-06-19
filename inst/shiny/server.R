library(shiny)
#runApp("~/Hackathon-RDWD-QualityMonitoring/R/shiny")

library(ggplot2)
library(data.table)
library(lubridate)
library(stringr)

#datafiles<-list.files("/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata/",pattern="daily",full.names = TRUE)
# datafiles<-list.files("/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata/",pattern="hourly",full.names = TRUE)

testfile<-"/home/dirksen/Hackathon-RDWD-QualityMonitoring/data/testdata//DeBilt_260_H_hourly_precip.csv"
Bilt<-fread(testfile)
names(Bilt)<-c("day","hour","P")


Bilt$hour<-str_pad(as.integer(Bilt$hour), width = 6, pad = "0")
Bilt$time<-paste0(Bilt$day," ",Bilt$hour)
Bilt$time<-as.POSIXct(Bilt$time,format="%Y%m%d %H%M%S")
# Bilt$day<-as.Date(Bilt$day,format="%Y%m%d")

p<-ggplot(Bilt,aes(time,P))+
  geom_point() 

server<-function(input, output, session){
  
  detection_datetime <- Sys.time()
  
  rv <- reactiveValues(showDetails = "false", station = "")
  
  setDetails <- function(detailVisible, station) {
    if (detailVisible) {
      rv$showDetails <- "true"
      rv$station <- station
    } else {
      rv$showDetails <- "false"
      rv$station <- ""
    }
  }
  
  observeEvent(input$showDetailsNL, setDetails(TRUE, "NL"))
  observeEvent(input$showDetailsNLa, setDetails(TRUE, "NL"))
  observeEvent(input$showDetails260a, setDetails(TRUE, "260"))
  observeEvent(input$showDetails280, setDetails(TRUE, "280"))
  observeEvent(input$showDetails280a, setDetails(TRUE, "280"))
  observeEvent(input$hideDetails, setDetails(FALSE))
  
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
  
  output$plot<-renderPlot({
    if (input$go == 0)
      return()
    
    isolate(
    p
    )
  })

  outputOptions(output, "showDetails", suspendWhenHidden = FALSE)
  outputOptions(output, "stationId", suspendWhenHidden = FALSE)

}