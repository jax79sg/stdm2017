## Stores all the functions created for assignment.

## MSE computation function
generateMSE=function(obs,pred)
{
  require(hydroGOF)
  return(hydroGOF::mse  (obs,pred))
}

## MAE computation function
generateMAE=function(obs,pred)
{
  require(hydroGOF)
  return(hydroGOF::mae  (obs,pred))
}

## MAPE computation function
generateMAPE=function(obs,pred)
{
  cat("\nSize is: ",length(obs)," vs ", length(pred),"\n")
  flush.console()
  if (length(obs)==length(pred) )
  {
    n=length(obs)
    cat("Size is: ",n)
    counter=1
    currentsum=0
    while(counter<=n)
    {
      At=obs[counter]
      Ft=pred[counter]
      #cat("\nAt:",At,"Ft",Ft,"\n")
      currentvalue=abs((At-Ft)/At)
      currentsum=currentsum+currentvalue
      counter=counter+1
    }
    mapeResults=(100/n)*currentsum
    if (is.nan(mapeResults))
    {
      mapeResults=0
    }
  }
  else
  {
    print("Error: Length of OBS and PRED must be identical")
  }
  
  return (mapeResults)
}



## To compute MSE and MAE from obs and pred and return.
## adjMetric (Mainly used to identify col names, or link ids)
generateErrorMetrics=function(obs,pred,filename,adjMetric)
{
  linkindex=1
  nooflinkes=dim(obs)[2]
  errorcompute=c()
  while(linkindex<=nooflinkes)
  {
    linkid=colnames(adjMetric)[linkindex]
    tempmse=generateMSE(obs[,linkindex],pred[,linkindex])
    tempmae=generateMAE(obs[,linkindex],pred[,linkindex])
    tempmape=generateMAPE(obs[,linkindex],pred[,linkindex])
    cat("\nlinkid:",linkid,"tempmse:",tempmse,"tempmae:",tempmae,"tempmape:", tempmape,"\n")
    errorcompute=c(errorcompute,linkid,tempmse,tempmae, tempmape)
    
    #cat("\n",linkid,tempmse,tempmae, length(errorcompute))
    linkindex=linkindex+1
  }
  errorcomputematrix=matrix(errorcompute,nrow=nooflinkes,ncol=4,byrow = T)
  colnames(errorcomputematrix)=c("Link","MSE","MAE","MAPE")
  print(errorcomputematrix)
  savedatatocsv(filename =filename,df = errorcomputematrix, isAppend = F)
  
}

#Split up dataset into 80% split set
getTrainSet=function(data,percentTrain=80)
{
  cat("\nTrainfunction class:", class(data))
  datasize=dim(data)[1]
  cat("\nDatasize:",datasize)
  trainsize=80/100*datasize
  cat("\nComputed Train size:",trainsize)    
  traindata=head(data,trainsize)
  devdata=tail(data,datasize-trainsize)
  cat("\nTrain size:",dim(traindata))
  cat("\nDev size:",dim(devdata))
  cat("\nCombined size:",dim(traindata)[1]+dim(devdata)[1])
  return (traindata)
}

#Split up dataset into 20% split set
getDevSet=function(data,percentTrain=80)
{
  cat("\nTrainfunction class:", class(data))
  datasize=dim(data)[1]
  cat("\nDatasize:",datasize)
  trainsize=80/100*datasize
  cat("\nComputed Train size:",trainsize)    
  traindata=head(data,trainsize)
  devdata=tail(data,datasize-trainsize)
  cat("\nTrain size:",dim(traindata))
  cat("\nDev size:",dim(devdata))
  cat("\nCombined size:",dim(traindata)[1]+dim(devdata)[1])
  return (devdata)
}



## Extract the UJT data set into weekday timings 
## Mon 6am to Thu 9pm
pruneWeekdayDataset=function()
{
  week1=c(361:1080)
  week2=c(1621:2340)
  week3=c(2881:3600)
  week4=c(4141:4860)
  colweekday1=c(week1, recursive=T)
  colweekday2=c(week2, recursive=T)
  colweekday3=c(week3, recursive=T)
  colweekday4=c(week4, recursive=T)
  weekdayscol=c(colweekday1,colweekday2,colweekday3,colweekday4)
  dim(weekdayscol)
  weekday1=matrix(UJT[week1,],nrow=dim(UJT[week1,])[1],ncol=dim(UJT[week1,])[2])
  dim(weekday1)
  weekday2=matrix(UJT[week2,],nrow=dim(UJT[week2,])[1],ncol=dim(UJT[week2,])[2])
  dim(weekday2)
  weekday3=matrix(UJT[week3,],nrow=dim(UJT[week3,])[1],ncol=dim(UJT[week3,])[2])
  dim(weekday3)
  weekday4=matrix(UJT[week4,],nrow=dim(UJT[week4,])[1],ncol=dim(UJT[week4,])[2])
  dim(weekday4)
  weekdaysdata=rbind(weekday1,weekday2,weekday3, weekday4)
  dim(weekdaysdata)
  rownames(weekdaysdata)=c(weekdayscol)
  colnames(weekdaysdata)=colnames(UJT)
  return(weekdaysdata)
}

## Extract the UJT data set into weekend timings 
## Fri 6am to Sun 9pm
pruneWeekendDataset=function()
{
  week1=c(1:360)
  week2=c(1081:1620)
  week3=c(2341:2880)
  week4=c(3601:4140)
  week5=c(4861:5400)
  colweekend1=c(week1, recursive=T)
  colweekend2=c(week2, recursive=T)
  colweekend3=c(week3, recursive=T)
  colweekend4=c(week4, recursive=T)
  colweekend5=c(week5, recursive=T)
  weekendcols=c(colweekend1,colweekend2,colweekend3,colweekend4,colweekend5)
  dim(weekdayscol)
  weekend1=matrix(UJT[week1,],nrow=dim(UJT[week1,])[1],ncol=dim(UJT[week1,])[2])
  dim(weekend1)
  weekend2=matrix(UJT[week2,],nrow=dim(UJT[week2,])[1],ncol=dim(UJT[week2,])[2])
  dim(weekend2)
  weekend3=matrix(UJT[week3,],nrow=dim(UJT[week3,])[1],ncol=dim(UJT[week3,])[2])
  dim(weekend3)
  weekend4=matrix(UJT[week4,],nrow=dim(UJT[week4,])[1],ncol=dim(UJT[week4,])[2])
  dim(weekend4)
  weekend5=matrix(UJT[week5,],nrow=dim(UJT[week5,])[1],ncol=dim(UJT[week5,])[2])
  dim(weekend5)
  
  weekendsdata=rbind(weekend1,weekend2,weekend3, weekend4,weekend5)
  dim(weekendsdata)
  rownames(weekendsdata)=c(weekendcols)
  colnames(weekendsdata)=colnames(UJT)
  return (weekendsdata)
}


#Prune dataset into days
pruneDailyDataset=function()
{
  day6=c(1:180,1261:1440,2520:2701,3960:4141)
  day7=c(1441:1620,2701:2880,3961:4140,5221:5400)
  day1=c(361:540,1621:1800,2881:3060,4141:4320)
  day2=c(541:720,1801:1980, 3061:3240, 4321:4500)
  day3=c(721:900,1981:2160, 3241:3420, 4501:4680)
  day4=c(901:1080,2161:2340,3421:3600, 4681:4860)
  day5=c(1081:1260, 2341:2520, 3601:3780, 4861:5040)
  
  day1m=matrix(UJT[day1,],nrow=dim(UJT[day1,])[1],ncol=dim(UJT[day1,])[2])
  colnames(day1m)=colnames((UJT))
  day2m=matrix(UJT[day2,],nrow=dim(UJT[day2,])[1],ncol=dim(UJT[day2,])[2])
  colnames(day2m)=colnames((UJT))
  day3m=matrix(UJT[day3,],nrow=dim(UJT[day3,])[1],ncol=dim(UJT[day3,])[2])
  colnames(day3m)=colnames((UJT))
  day4m=matrix(UJT[day4,],nrow=dim(UJT[day4,])[1],ncol=dim(UJT[day4,])[2])
  colnames(day4m)=colnames((UJT))
  day5m=matrix(UJT[day5,],nrow=dim(UJT[day5,])[1],ncol=dim(UJT[day5,])[2])
  colnames(day5m)=colnames((UJT))
  day6m=matrix(UJT[day6,],nrow=dim(UJT[day6,])[1],ncol=dim(UJT[day6,])[2])
  colnames(day6m)=colnames((UJT))
  day7m=matrix(UJT[day7,],nrow=dim(UJT[day7,])[1],ncol=dim(UJT[day7,])[2])
  colnames(day7m)=colnames((UJT))
  
  return (list(day1=day1m,day2=day2m,day3=day3m,day4=day4m,day5=day5m,day6=day6m,day7=day7m))
}

## Prune the UJT matrix to the ones in the list
pruneUJTMatrix=function(selectedlinks, originalMatrix)
{
  allColinOriginalMatrix=colnames(originalMatrix)
  #cat("Names in original matrix: ", allColinOriginalMatrix)
  tempContent=c()
  for (colitem in selectedlinks)
  {
    for(rowitem in selectedlinks)
    {
      #cat("\nProcessing [", rowitem,colitem,"]")
      if (rowitem %in% allColinOriginalMatrix)
      {
        #cat ("> Found!\n")
        tempContent=c(tempContent,originalMatrix[toString(rowitem),toString(colitem)])  
      }
      else
      {
        cat ("> NOT FOUND!... Set Zero\n")
        tempContent=c(tempContent,0)  
      }
      
    }
  }
  tempMatrix=matrix(tempContent,nrow=length(selectedlinks), ncol=length(selectedlinks))
  colnames(tempMatrix)=selectedlinks
  rownames(tempMatrix)=selectedlinks
  return(tempMatrix)
}


## Function to convert elements in list to str elements
listToString=function(mylist)
{
  tempnewlist=c()
  for (item in mylist)
  {
    tempnewlist=c(tempnewlist,toString(item))   
  }
  return (tempnewlist)
}


## Clean up all custom made values, data.
cleanupall=function()
{
  rm(selectedlinks,tempworkingdir)
}


## NOT USED
## Detect the trend of a time series graph
## WARNING: If there's a seasonality, the time cycle must be determined first, this becomes the input for order
detectTrend=function(df, name="NoNameGiven", verbose=F, order=0)
{
  library(forecast)
  trend_df=ma(df, order=order, centre = T)
  plot(as.ts(df))
  lines(trend_df)
  plot(as.ts(trend_df))
}

## Save dataframe as csv file
## if append=true, it will attempt to merge the data
## No check for duplicates
savedatatocsv=function(filename, df, isAppend=F)
{
  library(data.table)
  if (file.exists(filename))
  {
    if (isAppend)
    {
        dfRead<-read.csv(filename) # read the file
        all<-rbind(dfRead, df) # rbind both data.frames
        write.table(all,filename, row.names=F,na="NA",append=F, quote= FALSE, sep=",", col.names=T)      
    }
    else
    {
      #Over-writing file
      write.table(df,filename, row.names=F,na="NA",append=F, quote= FALSE, sep=",", col.names=T)            
    }
  }
  else
  {
    write.table(df,filename, row.names=F,na="NA",append=F, quote= FALSE, sep=",", col.names=T)      
  }

}

## Detect the seasonality of a time series graph
## Returns the no of seasons and the list of timings for each season
## http://stats.stackexchange.com/questions/16117/what-method-can-be-used-to-detect-seasonality-in-data
## https://anomaly.io/detect-seasonality-using-fourier-transform-r/
detectSeasonality=function(df, name="NoNameGiven", verbose=F)
{
  library(TSA)
  p = periodogram(df,plot = F)
  tempdd = data.frame(freq=p$freq, spec=p$spec)
  order = tempdd[order(-tempdd$spec),]
  top = head(order, length(order))  
  timesteps=1/top$f  
  seasonabilityresults=c(length(order))
  for (item in timesteps)
  {
    seasonabilityresults=c(seasonabilityresults,item)
  }
  if (verbose)
  {
    print("START------------------------------------------")
    cat("Seasonality check for ", name)
    print("Total no of seasonalities discovered:")
    print(length(order))

    print("Frequencies for each seasonality:")
    print(top)

    print("Time steps for each seasonality:")
    print(timesteps)
    print(seasonabilityresults)
    print("END------------------------------------------")    
  }
  return(seasonabilityresults)
}

## Extract seasonality info for each link
## Plot every column of data.frame (E.g. For every column, plot rows on x-axis and content of col on y axis)
plotbycol=function(df, isSaveToImage=T)
{
  tempNoOfCol=ncol(df)
  #cat("No of col: ", tempNoOfCol)
  tempCount=1
  while (tempCount<=tempNoOfCol)
  {
    tempseasonabilitybresults=detectSeasonality(df[,tempCount],paste("Link",colnames(df)[tempCount]))
    tempseasonabilitybresultstext=paste("Total no of seasonalities discovered: ",tempseasonabilitybresults[1],"\nTime steps for each seasonality: ",sep = "")
    tempcounter=2
    while (tempcounter<=tempseasonabilitybresults[1]+1)
    {
      tempseasonabilitybresultstext=paste(tempseasonabilitybresultstext,round(tempseasonabilitybresults[tempcounter], digits=2),", ",sep="")
      tempcounter=tempcounter+1
    }    
    if (isSaveToImage)
    {
      png(file=paste("Link",colnames(df)[tempCount],".png", sep=""),width=1680,height=1050)            
    }
    plot((df[,tempCount]), main=paste("Link ",colnames(df)[tempCount], sep=""), type='l', xlab="Time steps", ylab="Seconds/Metre", col='blue')
    mtext(tempseasonabilitybresultstext,cex = 2, side=3, line=-4.3, col='red')        
    if (isSaveToImage)
    {
      dev.off()
    }
    
    tempmatrix=matrix(c(colnames(df)[tempCount],round(tempseasonabilitybresults[2], digits=2),round(tempseasonabilitybresults[3], digits=2)),nrow=1,ncol=3, byrow=T)
    colnames(tempmatrix)=c("link","Period1","Period2")
    savedatatocsv("seasonalitydata.csv",data.frame(tempmatrix),T)
    
    tempCount=tempCount+1
  }
}

## Plot each UJT col's ACF
plotacfbycol=function(df, isSaveToImage=T, maxlag)
{
  tempNoOfCol=ncol(df)
  tempCount=1
  while (tempCount<=tempNoOfCol)
  {
    tempcounter=2
    if (isSaveToImage)
    {
      png(file=paste("ACF_Link",colnames(df)[tempCount],".png", sep=""),width=1680,height=1050)            
    }
    acf(df[,tempCount], lag.max=maxlag, xlab="Lag",ylab="ACF", main="ACF Plot of daily traffic")
    if (isSaveToImage)
    {
      dev.off()
    }
    tempCount=tempCount+1
  }  
}



## Plot each item's ACF
plotacfbyitem=function(df, name, isSaveToImage=T, maxlag, freq=1)
{
  if (isSaveToImage)
  {
    png(file=paste("Link",name,"_ACFPlot.png", sep=""),width=1680,height=1050)            
  }
  acfts=acf(df, lag.max=maxlag, main="")
  acfts$lag=acfts$lag*freq
  (plot(acfts,xlab="Lag by time steps", ylab="ACF"))    
  title(paste("ACF Link ", name, sep=""), line=-2)
  
  if (isSaveToImage)
  {
    dev.off()
  }
}

## Plot each item's ACF
plotpacfbyitem=function(df, name, isSaveToImage=T, maxlag, freq=1)
{
  if (isSaveToImage)
  {
    png(file=paste("Link",name,"_PACFPlot.png", sep=""),width=1680,height=1050)            
  }
  pacfts=pacf(df, lag.max=maxlag, main="")
  pacfts$lag=pacfts$lag*freq
  (plot(pacfts,xlab="Lag by time steps", ylab="PACF"))    
  title(paste("PACF Link ", name, sep=""), line=-2)
  
  if (isSaveToImage)
  {
    dev.off()
  }
}

## Plot each UJT col's PACF
plotpacfbycol=function(df, isSaveToImage=T, maxlag)
{
  tempNoOfCol=ncol(df)
  print(tempNoOfCol)
  tempCount=1
  while (tempCount<=tempNoOfCol)
  {
    tempcounter=2
    if (isSaveToImage)
    {
      png(file=paste("PACF_Link",colnames(df)[tempCount],".png", sep=""),width=1680,height=1050)            
    }
    pacf(df[,tempCount], lag.max=maxlag, xlab="Lag",ylab="PACF", main="PACF Plot of daily traffic")
    if (isSaveToImage)
    {
      dev.off()
    }
    
    tempCount=tempCount+1
  }  
}

##Takes in a matrix and test all cols of data
## This just quickly displays the results for many datasets in UJT, not for getting the T/F. 
## For T/F, use stationarytestbyitem function
stationarytestbycol=function(df)
{
  tempNoOfCol=ncol(df)
  tempCount=1
  while (tempCount<=tempNoOfCol)
  {
    tempcounter=2
    cat("\nTest stationarity for link ", colnames(df)[tempCount])
    tempbox=Box.test(df[,tempCount],lag=1,type="Ljung-Box")$p.value<0.05
    tempadf=adf.test(df[,tempCount], alternative="stationary")$p.value<0.05
    tempkpssresults=kpss.test(df[,tempCount])$p.value>0.05    
    cat ("\nBox:",tempbox," Adf:", tempadf, "kpss:", tempkpssresults)    
    tempCount=tempCount+1
  }  
}

## Takes in a 3 single data set and test against 3 tests.
## Box (Box-Pierce/Ljung-Box test)
## ADF (Augmented-Dickey-Fuller Test)
## KPSS (Kwiatkowski-Phillips-Schmidt-Shin )
## Finally returns a T or F, assume must pass all stationarity (Lots of test give false positives, that's why)
stationarytestbyitem=function(df, name='NoNameGiven')
{
    library("tseries")
    print("Test stationarity for: ")
    print(name)
    tempbox=Box.test(df,lag=1,type="Ljung-Box")$p.value<0.05
    tempadf=adf.test(df, alternative="stationary")$p.value<0.05
    tempkpssresults=kpss.test(df)$p.value>0.05    
    PP.test(df)$p.value>0.05
    cat (paste("\nBox:",tempbox,"\nAdf:", tempadf, "\nkpss:", tempkpssresults,sep=""),filename=paste("Link",name,"_StationaryTest.txt",sep=""),sep="")
    return (tempkpssresults && tempbox && tempadf)
}


## Perform difference of all cols of data.
## Returns a seq of datasets
differencebycol=function(df,lag=0)
{
  tempdf=diff(df,lag=lag)
  return (tempdf)
}

## This is to help us decide if our model decision is correct so can type in report.
## Don't think we are allowed to use this function
automaticArima=function(item)
{
  library(forecast)
  auto.arima(item)
}

## Function to display the various STACF and STPACF to decide STARIMA model
starimaIdentification=function(timeseriesmatrix, spatialweightmatrix, isSaveToImage=F, weekdaysdata, labelling="")
{
  png(file=paste("STACF_5000lag",".png", sep="") ,width=1680,height=1050)  
  stacf(timeseriesmatrix,spatialweightmatrix,5000)  
  png(file=paste("STACF_500lag",".png", sep="") ,width=1680,height=1050)  
  stacf(timeseriesmatrix,spatialweightmatrix,500)  
  
  png(file=paste("STACF_180_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(timeseriesmatrix, lag = 180, differences=1),spatialweightmatrix,3000)  
  png(file=paste("STACF_1260_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(timeseriesmatrix, lag = 1260, differences=1),spatialweightmatrix,3000)  
  png(file=paste("STACF_1260_180_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(timeseriesmatrix, lag = 1260, differences=1), lag = 180, differences=1),spatialweightmatrix,3000)  
  png(file=paste("STACF_180_1260_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(timeseriesmatrix, lag = 180, differences=1), lag=1260, differences = 1),spatialweightmatrix,3000)  
  
  png(file=paste("STACF_180_1_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(timeseriesmatrix, lag = 180, differences=1), lag = 1, differences = 1),spatialweightmatrix,3000)  
  png(file=paste("STACF_1260_1_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(timeseriesmatrix, lag = 1260, differences=1), lag = 1, differences = 1),spatialweightmatrix,3000)  
  png(file=paste("STACF_1260_180_1_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(diff(timeseriesmatrix, lag = 1260, differences=1), lag = 180, differences=1), lag = 1, differences = 1),spatialweightmatrix,3000)  
  png(file=paste("STACF_180_1260_1_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(diff(timeseriesmatrix, lag = 180, differences=1), lag=1260, differences = 1), lag = 1, differences = 1),spatialweightmatrix,3000)  
  
  png(file=paste("STACF_1_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(timeseriesmatrix, lag = 1, differences=1),spatialweightmatrix,3000) 
  png(file=paste("STACF_1_lag_B",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(timeseriesmatrix, lag = 1, differences=1),spatialweightmatrix,30) 
  png(file=paste("STPACF_1_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stpacf(diff(timeseriesmatrix, lag = 1, differences=1),spatialweightmatrix,30) 
  
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=1,d=1,q=1))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=0,d=1,q=1))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=1,d=1,q=0))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=1,d=1,q=2))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=0,d=1,q=2))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=2,d=1,q=2))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=2,d=1,q=1))$NRMSE)
  mean((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=2,d=1,q=0))$NRMSE)
  
  nooflinks=dim(spatialweightmatrix)[1]
  while(linkindex<=nooflinks)
  {
    png(file=paste("HIST_",linkindex,".png", sep="") ,width=1680,height=1050)  
    hist((starima_fit(timeseriesmatrix[1:4140,],list(w1=spatialweightmatrix),p=2,d=1,q=2))$RES[,linkindex])
    linkindex=linkindex+1
  }  
  
}


#Normaise spatial weight
spatialweightnormalise=function(selectedAdjUJT)
{
  ## Spread weight assuming equal influence. Cannot use rowMeans as it cannot handle cases where a link has zero connections to other links.
  counter=1
  while(counter<=dim(selectedAdjUJT)[2])
  {
    print("RowPrint")
    print(selectedAdjUJT[counter,])
    rowmean=sum(selectedAdjUJT[counter,])
    if(rowmean!=0)
    {
      selectedAdjUJT[counter,]=selectedAdjUJT[counter,]/rowmean
    }
    print(rowmean)
    print(selectedAdjUJT[counter,])
    counter=counter+1
  }
  return(selectedAdjUJT)
}


## STARIMA prediction
## raw weekdaysdata (To reference col and row ids)
## selectedUJTWeekdays (Data)
## selectedAdjUJT (Spatial Matrix)
## isSaveToImage Save to file instead of display
## ARIMA p,d,q (Should be identified before calling this function)
## trainstartpoint  (Starting point of training from original dataset)
## trainendpoint (End point of training from original dataset) +1 refers to starting point of devset
## devendpoint (End point of dev from original dataset)
## Returns: NRMSE values
starimaPredict=function(weekdaysdata, selectedUJTWeekdays, selectedAdjUJT,isSaveToImage=T,p=0,d=0,q=0, trainstartpoint, trainendpoint,devendpoint, labelling="", plotstart=1, plotend=100 )
{
  cat("isSaveToImage:",isSaveToImage,"p:",p,"d:",d,"q:",q,"trainstartpoint:",trainstartpoint,"trainendpoint:",trainendpoint,"devendpoint",devendpoint)
  source("starima_package.R")
  cat("Row Summed")
  #W_fit=list(w1=selectedAdjUJT/rowSums(selectedAdjUJT))
  cat("\nclass(selectedAdjUJT)",class(selectedAdjUJT))
  
  # print("selectedAdjUJT")
  # for (item in selectedAdjUJT[1:5,])
  # {
  #   print(item)
  # }
  #cat("\nselectedAdjUJT:",selectedAdjUJT[1:5,])
  
  ## Spread weight assuming equal influence. Cannot use rowMeans as it cannot handle cases where a link has zero connections to other links.
  counter=1
  while(counter<=dim(selectedAdjUJT)[2])
  {
    print("RowPrint")
    print(selectedAdjUJT[counter,])
    rowmean=sum(selectedAdjUJT[counter,])
    if(rowmean!=0)
    {
      selectedAdjUJT[counter,]=selectedAdjUJT[counter,]/rowmean
    }
    print(rowmean)
    print(selectedAdjUJT[counter,])
    counter=counter+1
  }

  W_fit=list(w1=selectedAdjUJT)
  #fit.star=starima_fit(selectedUJTWeekdays[1:2304,],W_fit,p=p,d=d,q=q)
  print("\nSTARIMA fitting")
  timestart=Sys.time()
  fit.star=starima_fit(selectedUJTWeekdays[trainstartpoint:trainendpoint,],W_fit,p=p,d=d,q=q)
  timeend=Sys.time()
  timetaken=timeend-timestart
  print(timestart)
  print(timeend)
  print(timetaken)
  #pre.star=starima_pre(selectedUJTWeekdays[(2304-p-d-q+1):2880,],model=fit.star)
  #pre.star=starima_pre(selectedUJTWeekdays[(trainendpoint-p-d-q+1):devendpoint,],model=fit.star)
  print("\nSTARIMA predicting")
  pre.star=starima_pre(selectedUJTWeekdays[(trainendpoint-p-d-q+1):devendpoint,],model=fit.star)
  # cat("\nSize of devsetforpred:",2880-(2304-1-2+11) )
  # cat("\nSize of predictset:", dim(pre.star$PRE))
  # cat("\nSize of devset for check:",length(selectedUJTWeekdays[2305:2880,1]))
  # cat("\nlength(pre.star$PRE[,1]:",length(pre.star$PRE[,1]))
  
  nooflinks=dim(selectedAdjUJT)[1]
  linkindex=1
  while(linkindex<=nooflinks)
  {
    if(isSaveToImage)
    {
      png(file=paste(labelling,colnames(selectedAdjUJT)[linkindex],".png", sep=""),width=1680,height=1050)            
    }
    #matplot(selectedUJTWeekdays[(trainendpoint+1+plotstart):(trainendpoint+1+plotend),linkindex],type="l",ylab="second/metre",xlab = "Time Step")
    matplot(pre.star$OBS[plotstart:plotend,linkindex],type="l",ylab="second/metre",xlab = "Time Step")
    matplot(pre.star$PRE[plotstart:plotend,linkindex],type="l",add=T,col='red')
    legend("topright", inset=0., legend = c("Data","Predicted"), col=(1:2), lty=c(1,2))
    title(paste(labelling,colnames(selectedAdjUJT)[linkindex]  ,"\nTime Steps: ", trainendpoint+1+plotstart-1, " to ", trainendpoint+1+plotend))
    
    if(isSaveToImage)
    {
      dev.off()  
    }    
    
    linkindex=linkindex+1
  }
  
  return (pre.star)
}

## Export entire dataset into ESRI shapefiles (1404 links)
## Exported file to help create the shape files for the UJT links (256 links), from which the selected links was collected.
exportToShapeFile=function(dir,layername, overwrite=T)
{
  library(rgdal)
  writeOGR(obj=LCAPShp, dsn=dir, layer=layername, driver="ESRI Shapefile",overwrite_layer=overwrite) # this is in geographical projection
  
}

##Traffic flow across links at same timeframe
plotlinktraffic=function(selectedUJT,timestepranges,nooflinks)
{
  timestepranges=c(384:396,432:444,462:474)
  nooflinks=17
  currentcolor=rgb(runif(nooflinks),runif(nooflinks),runif(nooflinks))
  colorcodes=c(currentcolor)
  matplot(selectedUJT[timestepranges,1], type='l',col=currentcolor, xlab = "Time Steps", ylab="Seconds/Metre", main="Traffic flow across links at same timeframe",ylim = c(0,0.25))
  counter=2
  while(counter<=nooflinks)
  {
    currentcolor=rgb(runif(nooflinks),runif(nooflinks),runif(nooflinks))
    colorcodes=c(colorcodes,currentcolor)
    matplot(selectedUJT[timestepranges,counter], type='l',col=currentcolor, add = T)
    counter=counter+1
  }
  legend("topleft", legend = colnames(selectedUJT), col=colorcodes, lwd = 1)
  
}



## Mean of traffic flow across links
meanflowacrosslinks=function(selectedUJT)
{
  library(gplots)
  selectedUJT.rowmean=rowMeans(selectedUJT)
  return(selectedUJT.rowmean)
}

#Plot traffic flow for visual analysis
trendspotting=function(selectedUJT,selectedUJT.rowmean)
{
  old.par <- par(mfrow=c(1, 3))
  plot(selectedUJT[,14], type='l', xlab="Time Step", ylab="second/metre", main="Ttraffic flow across link 883", col='blue')
  abline(lm(selectedUJT[,14]~c(1:5400)), col='red')
  plot(selectedUJT[,11], type='l', xlab="Time Step", ylab="second/metre", main="Traffic flow across link 1770", col='blue')
  abline(lm(selectedUJT[,11]~c(1:5400)), col='red')
  plot(selectedUJT.rowmean, type='l', xlab="Time Step", ylab="second/metre", main="Mean traffic flow across links", col='blue')
  abline(lm(selectedUJT.rowmean~c(1:5400)), col='red')
  par(old.par)  
}

#Plot traffic flow for visual analysis
patternspotting=function(selectedUJT)
{
  e883=plot(selectedUJT[1:2520,14],type='l',xlab='Time steps\n2 weeks from Sat 6am',ylab="seconds/metre",main="Traffic flow for Link 883", col='blue')
  abline(v=c(180,360,540,720,900,1080), col='red', type='-', lty=2)
  abline(v=c(1260), col='green', type='-', lty=2)
  e1770=plot(selectedUJT[1:2520,11],type='l',xlab='Time steps\n2 weeks from Sat 6am',ylab="seconds/metre",main="Traffic flow for Link 1770", col='blue')
  abline(v=c(180,360,540,720,900,1080), col='red', type='-', lty=2)
}

#Detect seasonalities
seasonalitycheck=function(selectedUJT)
{
  counter=1
  results=c()
  resultscolnames=c("link","s1","s2","ss1","ss2")
  while(counter<=17)
  {
    
    seasonalities=detectSeasonality(selectedUJT[1:3780, counter])
    results=c(results,colnames(selectedUJT)[counter], seasonalities[2],seasonalities[3],seasonalities[2]*5/60,seasonalities[3]*5/60)
    counter=counter+1
  }
  mresults=matrix(results,nrow=17,ncol = 5, byrow = T)
  savedatatocsv(filename="seasons.csv",(mresults))
  
  
}


# Plot the different differencing
differencingPlot=function(selectedUJT)
{
  old.par <- par(mfrow=c(2, 2))
  png(file=paste("STACF_180_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(selectedUJT, lag = 180, differences=1),selectedAdjUJT,3000)  
  title(paste("Difference by lag 180", sep=""), line=-2)
  dev.off()
  png(file=paste("STACF_1260_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(selectedUJT, lag = 1260, differences=1),selectedAdjUJT,3000)  
  title(paste("Difference by lag 1260", sep=""), line=-2)
  dev.off()
  png(file=paste("STACF_1260_180_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(selectedUJT, lag = 1260, differences=1), lag = 180, differences=1),selectedAdjUJT,3000)  
  title(paste("Difference by lag 1260 and then 180", sep=""), line=-2)
  dev.off()
  png(file=paste("STACF_180_1260_lag",counter,".png", sep="") ,width=1680,height=1050)  
  stacf(diff(diff(selectedUJT, lag = 180, differences=1), lag=1260, differences = 1),selectedAdjUJT,3000)  
  title(paste("Difference by lag 180 and then 1260", sep=""), line=-2)
  dev.off()
}

