#' @title Break detection with Buishand method
#' @description Function for break detection in a yearly or seasonal series that uses the Buishand cumulative test. It needs a table with two columns (datetime and value). the input is temporarily downloaded by the dummy data
#' @author Antonello Squintu
#' @param id id
#' @param input input
buishand <- function(id,input){
  
  #Package "climtrends" is needed
  
  #Uploading of dummy series. Comment out when working in flow
  setwd("/home/squintu/Hackathon-RDWD-QualityMonitoring/data")
  #files <- readRDS("data/testdata.rda")
  #diff_files = files[which(grepl('stump',files))]
  #diff=read.table(diff_files)
  #input=read.table("Marknesse_diff_DJF.txt") 
  #Setting of the input table  
  diff=input
  colnames(diff)=c('datetime','diff')
  diff$time <- strptime(diff$datetime, format="%Y%m%d%H%M%S")
  diff$year <- as.integer(format((strptime(diff$datetime, format="%Y%m%d%H%M%S")-9*3600),'%Y'))
  
  #Buishand test
  bd_buishand=data.frame(year=diff$year)
  bd_buishand$score=round(BuishandRangeTest(diff$diff),digits=2)
  
  cv=Buishand.Critical.Values[max(which(length(diff$diff)>=as.numeric(row.names(Buishand.Critical.Values[1:5,])))),2]
  
  cy <- bd_buishand[abs(bd_buishand$score)>cv,]
  r=2
  bd_buishand$TF=FALSE
  
  #Research of local maxima
  for(i in 1:nrow(cy))
  {
    if (cy$year[i]>bd_buishand$year[1]+range && cy$year[i]<bd_buishand$year[nrow(bd_buishand)]-range )
    {
      aux <- bd_buishand[bd_buishand$year %in% (cy$year[i]-r):(cy$year[i]+r),]
      if (which.max(abs(aux$score))==r+1)
      {
        bd_buishand[bd_buishand$year==cy$year[i],'TF']=TRUE
      }
    }
  }
  
  plot(bd_buishand$year,bd_buishand$score,type='l',
       main=paste('Buishand Test',id),
       xlab='year',
       ylab='buishand score',
       ylim=c(min(-cv,bd_buishand$score),max(cv,bd_buishand$score)))
  points(bd_buishand$year,bd_buishand$score,pch=16)
  abline(h=cv,col='darkgreen',lwd=2)
  abline(h=-cv,col='darkgreen',lwd=2)
  abline(v=bd_buishand$year[bd_buishand$TF],col='red',lwd=2)
  
  breaks=list()
  breaks[as.character(id)]=bd_buishand[bd_buishand$TF,'year']
  
  return(breaks)
}
