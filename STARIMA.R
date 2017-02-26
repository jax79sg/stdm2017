rm(list=ls())
#tempworkingdir="/Users/jax/ucl/SpatialTemporalDataMining/assignment/starima"
#setwd(tempworkingdir)


## STARIMA calls
## Must call following lines before calling source("functions.R")
load("UJTWorkSpace.rdata")

## These are the selected links to analyse.
#selectedlinks_int=c(2,3,4,5,8,214,1,89,131,5,112,201)
#selectedlinks_int=c(2090,433,434,2344,1620,1447,417,1518,452,453,2112,2087,2102,446,2358,1614,1400,447,2084,1745,2363,1747,1622,432,1604,2357,2059,1407)
selectedlinks_int=c(2334,2240,2237,2236,2185,2184,2183,1872,1870,1771,1770,1214,1058,883,882,881,1506)
#selectedlinks_int=c(2090,433,434,2344,1620)
#selectedlinks_int=c(2344,2090,1447,1518,452,453,2102,2358,1614,447,2084,1745,1747,1622,2357,2059)
#selectedlinks_int=c(2344,2090)
#selectedlinks_int=c(434,2090)

source("functions.R")

cleanupall()
#exportToShapeFile(dir=tempworkingdir,layername="ALL_LINKS", overwrite=T)

## seq of selected links
selectedlinks=listToString(selectedlinks_int)

## Refine UJT to selected links only
selectedUJT=UJT[,selectedlinks]
selectedUJTWeekdays=pruneWeekdayDataset()[,selectedlinks]
##selectedUJTWeekends=pruneWeekendDataset()[,selectedlinks]

## Refine adj matrix to selected links only
selectedAdjUJT=pruneUJTMatrix(selectedlinks_int,LCAPAdj)

#source("starima_package.R")

# stmatrix=diff(selectedUJT%*%selectedAdjUJT, lag = 180, differences=1)
# acf(stmatrix, lag.max = 500)
# stacf(diff(selectedUJT, lag=180, differences = 1),selectedAdjUJT,5000)
## Performing STARIMA weekdays dataset only
# nrmseresults=starimaPredict(weekdaysdata, selectedUJTWeekdays, selectedAdjUJT,isSaveToImage=T,p=0,d=1,q=1, trainstartpoint=1, trainendpoint=2304,devendpoint=2880, labelling="STARIMA_PREDICTED_WEEKDAY_Link_ShortTermView", plotstart=1,plotend=500 )
# generateErrorMetrics(nrmseresults$OBS,nrmseresults$PRE,filename="WeekdaysErrorcomputematrix.csv",selectedAdjUJT)


## Performing STARIMA all week dataset only
#nrmseresults=starimaPredict(selectedUJT, selectedUJT, selectedAdjUJT,isSaveToImage=T,p=0,d=1,q=1, trainstartpoint=1, trainendpoint=4320,devendpoint=5400,labelling="STARIMA_PREDICTED_ALLWEEK_Link_ShortTermView" , plotstart=1,plotend=180)
nrmseresults=starimaPredict(selectedUJT, selectedUJT, selectedAdjUJT,isSaveToImage=T,p=2,d=1,q=2, trainstartpoint=1, trainendpoint=4140,devendpoint=5400,labelling="STARIMA_PREDICTED_ALLWEEK_Link_ShortTermView" , plotstart=1,plotend=180)
generateErrorMetrics(nrmseresults$OBS,nrmseresults$PRE,filename="AllWeekErrorcomputematrix.csv",selectedAdjUJT)



# nrmseresults$OBS[2]
# nrmseresults$PRE[2]
