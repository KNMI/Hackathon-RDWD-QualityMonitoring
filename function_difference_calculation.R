#Function for calculation of difference between automatic and manual series in yearly and seasonal precipitation series
#Needs to be completed

#Reading the dummy series
setwd("/home/squintu/Hackathon-RDWD-QualityMonitoring")
files <- readRDS("data/testdata.rda")
seasonal_files = files[which(grepl('seasonal',files))]

#First dummy file has yearly and seasonal data from Almelo (automatic)
series_aut <- read.table(seasonal_files[1], sep=',')
colnames(series_aut)= c('year','y_aut','djf_aut','mam_aut','jja_aut','son_aut')

#Needing series with manual measurements, for the moment Doetinchem autmoatic is used
series_man <- read.table(seasonal_files[2], sep=',')
colnames(series_man)= c('year','y_man','djf_man','mam_man','jja_man','son_man')

#Select years in common between the two series
years_man=series_man$year
years_aut=series_aut$year
years=years_man[years_man %in% years_aut]
series_man=series_man[series_man$year%in%years,]
series_aut=series_aut[series_aut$year%in%years,]

#Create the yearly and seasonal difference between the two series
merged=merge(series_man, series_aut, by='year')
diff=data.frame(year=merged$year, y=merged$y_man-merged$y_aut, djf=merged$djf_man-merged$djf_aut,
                mam=merged$mam_man-merged$mam_aut, jja=merged$jja_man-merged$jja_aut, son=merged$son_man-merged$son_aut)



