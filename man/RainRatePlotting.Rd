% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rainratePlotting.R
\name{RainRatePlotting}
\alias{RainRatePlotting}
\title{Rain rate plotting}
\usage{
RainRatePlotting(timeseries1, timeseries2, averagedtimeseries1,
  averagedtimeseries2, timespan1 = NULL, timespan2 = NULL)
}
\arguments{
\item{timeseries1}{a list of timeseries, in data.table format containing datetime and value columns, that are to be compared with a validation selection of stations. Each element in the list has the name of its seriesID.}

\item{timeseries2}{a list of timeseries, in data.table format containing datetime and value columns, that make up the validation selection of stations. Each element in the list has the name of its seriesID.}

\item{averagedtimeseries1}{the averaged timeline of the subset in timelines 1, as data.table containing datetime and value columns. Note that this is the output from the average.spatial function.}

\item{averagedtimeseries2}{the averaged timeline of the subset in timelines 2, as data.table containing datetime and value columns. Note that this is the output from the average.spatial function.}

\item{timespan1}{lower bound value in as.Date format that span up the range over which the timeseries should be plotted. If not provided, the entire period provided in the input data is plotted.}

\item{timespan2}{upper bound value in as.Date format that span up the range over which the timeseries should be plotted. If not provided, the entire period provided in the input data is plotted.}
}
\description{
Visualisation function of rain rate timelines of the selection of stations and their average.
}
\details{
When selecting subsets of stations that should be compared, the rain rate of each individual station as well as the overall averaged rain rate of both subsets are plotted in time. 
#@example RainRatePlotting(timeseries1, timeseries2, averagedtimeseries1, averagedtimeseries2, timespan1=as.Date("20000101", format="%Y%m%d"), timespan2=as.Date("20150101", format="%Y%m%d"))
}
\author{
Lotte
}
