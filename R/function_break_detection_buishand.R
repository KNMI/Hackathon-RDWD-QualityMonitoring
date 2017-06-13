#' Break detection
#' @title Break detection with Buishand method
#' @description Function for break detection in a yearly or seasonal series that uses the Buishand cumulative test. It needs a table with two columns (year and value). the input is temporarily downloaded by the dummy data
#' @param df.difference An input table with two columns: year and difference
#' @author Antonello Squintu


break.detection <- function(df.difference){

#Package "climtrends" is needed
library("climtrends")

#Uploading of dummy series. Comment out when working in flow
# setwd("/home/squintu/Hackathon-RDWD-QualityMonitoring")
# files <- readRDS("data/testdata.rda")
# diff_files = files[which(grepl('stump',files))]
# diff=read.table(diff_files)
  
#Setting of the input table  
diff=df.difference
colnames(diff)=c('year','diff')
diff$year=as.numeric(diff$year)


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
     main='Buishand Test',
     xlab='year',
     ylab='buishand score')
points(bd_buishand$year,bd_buishand$score,pch=16)
abline(h=cv,col='darkgreen',lwd=2)
abline(h=-cv,col='darkgreen',lwd=2)
abline(v=bd_buishand$year[bd_buishand$TF],col='red',lwd=2)

breaks=bd_buishand[bd_buishand$TF,'year']

return(breaks)
}
