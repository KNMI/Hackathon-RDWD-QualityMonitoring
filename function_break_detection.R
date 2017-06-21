#Function for break detection in a yearly or seasonal series
#Needs as input a file with two columns
#Column 1: date that indicates the end of the measuring interval
#Column 2: value (can be yearly or seasonal)

#Package "snht" is needed
library("snht")
library("climtrends")

#Uploading of dummy series. Comment out when working in flow
setwd("/home/squintu/Hackathon-RDWD-QualityMonitoring")
files <- readRDS("data/testdata.rda")
diff_files = files[which(grepl('stump',files))]
diff=read.table(diff_files)
colnames(diff)=c('year','diff')
diff$year=as.numeric(diff$year)

#Create a table where the detected breaks are stored

BreakDetection=data.frame(year=diff$year)

#Upload files from database. To be written. First column must be called 'year', second column must be called 'diff'.




# #Robust SNHT (it can't find breaks in the tails of the time series)
# 
# bd_rsnht=robustSNHT(diff$diff, period=5)
# bd_rsnht$score=round(bd_rsnht$score,digits=2)
# bd_rsnht=cbind(year=diff$year,bd)
# 
# plot(bd_rsnht$year,bd_rsnht$score,type='l',
#      main='Robust SNHT Test',
#      xlab='year',
#      ylab='snht score')
# points(bd_rsnht$year,bd_rsnht$score,pch=16)
# abline(h=6.8,col='darkgreen',lwd=2)
# abline(v=bd_rsnht[which(bd_rsnht$score>6.8),'year'],col='red',lwd=2)
# 
# BreakDetection$Rsnht=0
# BreakDetection[BreakDetection$year==bd_rsnht[which(bd_rsnht$score>6.8),'year'],'Rsnht']=1

#Buishand test


bd_buishand=data.frame(year=diff$year)
bd_buishand$score=round(BuishandRangeTest(diff$diff),digits=2)

cv=Buishand.Critical.Values[max(which(length(diff$diff)>=as.numeric(row.names(Buishand.Critical.Values[1:5,])))),2]

cy <- bd_buishand[abs(bd_buishand$score)>cv,]
r=2
bd_buishand$TF=FALSE
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


# cy=data.frame(year=bd_buishand[which(abs(bd_buishand$score)>cv),'year'])
# cy$year.shifted=NA
# cy$year.shifted[(2:length(cy$year))]=cy$year[1:(length(cy$year)-1)]
# cy$inc <- abs(cy$year-cy$year.shifted) >= 2
# cy$groups <- 0
# #cy.groups=list()
# apply(cy, 1, function(x){
#   return(x)
#   #abs(x$year - year$shifted)
# })
# cy.final=NULL
# j=1
# start=1
# for (i in 2:dim(cy)[1])
# {
#   if (cy$inc[i]>2)
#   {
#    stop=i-1
#     aux=data.frame(year=cy$year[start:stop],score=bd_buishand[bd_buishand$year %in% (cy[start,'year']:cy[stop,'year']),'score'])
#     cy.final[j,]<-aux[which.max(aux$score),]
#     j=j+1
#     start=i
#   }
# }
# stop=dim(cy)[1]
# aux=data.frame(year=cy$year[start:stop],score=bd_buishand[bd_buishand$year %in% (cy[start,'year']:cy[stop,'year']),'score'])
# cy.final[j,]<-aux[which.max(aux$score),]
# cy.final=lapply(cy.groups, function(x){})

plot(bd_buishand$year,bd_buishand$score,type='l',
     main='Buishand Test',
     xlab='year',
     ylab='buishand score')
points(bd_buishand$year,bd_buishand$score,pch=16)
abline(h=cv,col='darkgreen',lwd=2)
abline(h=-cv,col='darkgreen',lwd=2)
abline(v=bd_buishand$year[bd_buishand$TF],col='red',lwd=2)

BreakDetection$buishand=0
BreakDetection[BreakDetection$year %in% bd_buishand[bd_buishand$TF,'year'],'buishand']=1

#Pettitt test

bd_pettitt=data.frame(year=diff$year)
bd_pettitt$score=round(PettittTest(diff$diff),digits=2)

cv=Pettitt.Critical.Values[max(which(length(diff$diff)>=as.numeric(row.names(Pettitt.Critical.Values[,])))),1]

cy <- bd_pettitt[abs(bd_pettitt$score)>cv,]
r=2
bd_pettitt$TF=FALSE
for(i in 1:nrow(cy))
{
  if (cy$year[i]>bd_pettitt$year[1]+range && cy$year[i]<bd_pettitt$year[nrow(bd_pettitt)]-range )
  {
    aux <- bd_pettitt[bd_pettitt$year %in% (cy$year[i]-r):(cy$year[i]+r),]
    if (which.max(abs(aux$score))==r+1)
    {
      bd_pettitt[bd_pettitt$year==cy$year[i],'TF']=TRUE
    }
  }
}

plot(bd_pettitt$year,bd_pettitt$score,type='l',
     main='Pettitt Test',
     xlab='year',
     ylab='pettitt score')
points(bd_pettitt$year,bd_pettitt$score,pch=16)
abline(h=cv,col='darkgreen',lwd=2)
abline(h=-cv,col='darkgreen',lwd=2)
abline(v=bd_pettitt$year[bd_pettitt$TF],col='red',lwd=2)

BreakDetection$pettitt=0
BreakDetection[BreakDetection$year %in% bd_pettitt[bd_pettitt$TF,'year'],'pettitt']=1

#SNHT

bd_snht=data.frame(year=diff$year)
bd_snht$score=round(PettittTest(diff$diff),digits=2)/10

cv=SNHT.Critical.Values[max(which(length(diff$diff)>=as.numeric(row.names(SNHT.Critical.Values[,])))),4]

cy <- bd_snht[abs(bd_snht$score)>cv,]
r=2
bd_snht$TF=FALSE
for(i in 1:nrow(cy))
{
  if (cy$year[i]>bd_snht$year[1]+range && cy$year[i]<bd_snht$year[nrow(bd_snht)]-range )
  {
    aux <- bd_snht[bd_snht$year %in% (cy$year[i]-r):(cy$year[i]+r),]
    if (which.max(abs(aux$score))==r+1)
    {
      bd_snht[bd_snht$year==cy$year[i],'TF']=TRUE
    }
  }
}

plot(bd_snht$year,bd_snht$score,type='l',
     main='snht Test',
     xlab='year',
     ylab='snht score')
points(bd_snht$year,bd_snht$score,pch=16)
abline(h=cv,col='darkgreen',lwd=2)
abline(h=-cv,col='darkgreen',lwd=2)
abline(v=bd_snht$year[bd_snht$TF],col='red',lwd=2)

BreakDetection$snht=0
BreakDetection[BreakDetection$year %in% bd_snht[bd_snht$TF,'year'],'snht']=1