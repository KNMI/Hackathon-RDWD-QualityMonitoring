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
  
  output$summary<-renderTable({
    quantile(Bilt$P)
  })
  
  output$plot<-renderPlot({
     p
  })
  
  
}