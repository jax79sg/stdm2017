## ARIMA.R
rm(list=ls())
load("UJTWorkSpace.rdata")
tempworkingdir="temp"
# setwd(tempworkingdir)
## These are the selected links to analyse.
#selectedlinks_int=c(2,3,4,5,8,214,1,89,131,5,112,201)
selectedlinks_int=c(2090,433,434,2344,1620,1447,417,1518,452,453,2112,2087,2102,446,2358,1614,1400,447,2084,1745,2363,1747,1622,432,1604,2357,2059,1407)
#selectedlinks_int=c(2059,1447)
#selectedlinks_int=c(2344,1447,1518,452,453,2102,2358,1614,447,2084,1745,1747,1622,2357,2059)





## STARIMA calls
source("functions.R")
#exportToShapeFile(dir=tempworkingdir,layername="ALL_LINKS", overwrite=T)

## seq of selected links
selectedlinks=listToString(selectedlinks_int)

## Refine UJT to selected links only
selectedUJT=UJT[,selectedlinks]
selectedUJTWeekdays=pruneWeekdayDataset()[,selectedlinks]
#selectedUJTWeekends=pruneWeekendDataset()[,selectedlinks]
selectedUJTMondays=(pruneDailyDataset()$day1)[,selectedlinks]


## Refine adj matrix to selected links only
selectedAdjUJT=pruneUJTMatrix(selectedlinks_int,LCAPAdj)
#autoarima=automaticArima(selectedUJT[,2])



runArima=function(df, frequency=1, trainstart=1, trainend=4140,devstart=4141,devend=5400, pred_ahead=1260, isSaveToImage=F)
{
  require(forecast)
  arimamodels=c()
  arimamodelscolumnnames=c("link","p","d","q", "P","D","Q","Period","AIC", "BIC", "MSE", "MAE")
  
  
  nooflinks=dim(df)[2]
  linkindex=1
  while(linkindex<=nooflinks)
  {
    linkid=colnames(df)[linkindex]
    tsdata=ts(df[trainstart:trainend,linkindex],frequency=frequency)
    stationarytestbyitem(diff(tsdata, lag = frequency),name=linkid)
    
    if (isSaveToImage)
    {
      png(file=paste("Link",linkid,"_Plot.png", sep=""),width=1680,height=1050)            
    }
    plot(tsdata,xlab="Weeksdays (Mon to Thu)",ylab="seconds/metre",col="blue")
    #plot(decompose(tsdata))
    title(paste("Timeseries of Link ",linkid,sep=""))
    if(isSaveToImage)
    {
      dev.off()  
    }
    
          
    
    
    
    plotacfbyitem(tsdata,freq = frequency,name = linkid,isSaveToImage = isSaveToImage,maxlag = 30)
    plotpacfbyitem(tsdata,freq = frequency,name = linkid,isSaveToImage = isSaveToImage,maxlag = 30)
    plotacfbyitem(diff(tsdata),freq = frequency,name = paste(linkid,"_diff",frequency,sep=""),isSaveToImage = isSaveToImage,maxlag = 30)
    plotpacfbyitem(diff(tsdata),freq = frequency,name = paste(linkid,"_diff",frequency,sep=""),isSaveToImage = isSaveToImage,maxlag = 30)

    
    arimafit=auto.arima(tsdata, approximation=T,trace=T, D=1, max.order=5, stepwise=T)
    #arimafit=auto.arima(df[trainstart:trainend,linkindex], approximation=F,trace=T, max.order=8, stepwise=F)
    p=arimafit$arma[1]
    q=arimafit$arma[2]
    d=arimafit$arma[6]
    P=arimafit$arma[3]
    Q=arimafit$arma[4]
    D=arimafit$arma[7]
    Period=arimafit$arma[5]
    
    pred=predict(arimafit,n.ahead=pred_ahead)
    
    if(isSaveToImage)
    {
      png(file=paste("Link",linkid,"_7dayPredictionPlot.png", sep=""),width=1680,height=1050)              
    }
    matplot(ts(selectedUJT[devstart:devend,linkindex],frequency=frequency), type='l',xlab="Time steps (Last 7 days)", ylab="seconds/metre")
    matplot(pred$pred[1:pred_ahead],col='red',type='l',add=T)
    legend("topright", inset=0., legend = c("Data","Predicted"), col=(1:2), lty=c(1:2))
    title(paste("Link ", linkid, "\nARIMA Prediction (",pred_ahead,"-steps-ahead) ", sep=""))
    if(isSaveToImage)
    {
      dev.off()  
    }
    
    mse=generateMSE(selectedUJT[devstart:devend,linkindex],pred$pred[1:pred_ahead])
    mae=generateMAE(selectedUJT[devstart:devend,linkindex],pred$pred[1:pred_ahead])
    
    arimamodels=c(arimamodels,linkid,p,d,q,P,D,Q,Period,arimafit$aic,arimafit$bic, mae, mse)
    
    
    cat(file=paste("link",linkid,"_predictiondata.csv", sep=""),pred$pred,sep=",")
    
    linkindex=linkindex+1
  }
  arimamodelsmatrix=matrix(arimamodels,nrow=nooflinks,ncol = length(arimamodelscolumnnames), byrow = T)
  colnames(arimamodelsmatrix)=arimamodelscolumnnames
  savedatatocsv(arimamodelsmatrix, filename=paste("Alllinks_modeldetailsandresults.csv", sep=""))
}





#runArima(selectedUJT, frequency=180, trainstart=1, trainend=4140,devstart=4141,devend=5400, pred_ahead=1260, isSaveToImage=T)
runArima(selectedUJTWeekdays, frequency=180, trainstart=1, trainend=2340,devstart=2341,devend=2880, pred_ahead=540, isSaveToImage=T)

require(gplots)
decompose(selectedUJT[1:5400,1])
plot(selectedUJT[1:3780,1],type='l')
plot(diff(selectedUJT[1:900,1], differences = 1),type='l')
abline(v=150, col="red")
abline(v=300, col="red")
abline(v=450, col="red")
abline(v=600, col="red")
detectSeasonality(selectedUJT[1:3780,1])

plotmeans(c(1:5400)~diff(selectedUJT[1:3780,1], differences = 1))
tsdisplay(selectedUJT[,1])












## Using DSHW
x=msts(selectedUJT[1:5400,1],seasonal.periods = c(180,1440))
fitme=dshw(x)
pred=predict(fitme,h=180)
matplot(selectedUJT[1:5400,1],type='l')
matplot(pred$modelhow t,type='l',add = T,col='red',xlim=c(5400,6300))
matplot(fitme$fitted[1:5400],type='l',add = T,col='red',xlim=c(5400,6300))

bats=tbats(selectedUJT[1:5400,1], seasonal.periods = c(180,1440), use.trend=TRUE, use.parallel=TRUE)
plot(bats)
# 
# ## Using BATS
# x=msts(selectedUJT[,1],seasonal.periods = c(1350,2700))
# fitme=bats(x, use.parallel=T)
# plot(forecast(fitme,h=2700))
# 
## Using TBATS
# require(forecast)
# x=msts(selectedUJTWeekdays[,1],seasonal.periods = c(180,1440))
# fitme=tbats(x,use.parallel=T)
# plot(forecast(fitme,h=900))

y=ts(selectedUJTWeekdays[,1],f=180)
fit=arima(y,order=c(1,1,1), xreg=fourier(y, K=4))
plot(forecast(fit,h=2*180,xreg=fourier(y,K=4, h=2*180)))

#fit=arima(selectedUJTWeekdays[,1],order=c(1,1,1), seasonal=list(order=c(1,1,0), period=180), optim.method="Nelder-Mead")

