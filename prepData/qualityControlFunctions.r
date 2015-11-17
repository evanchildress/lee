checkLengthWeight<-function(xlim=c(0,max(allTroutData$length,na.rm=T)),
                            ylim=c(0,max(allTroutData$weight,na.rm=T))){
  badLength<-which(allTroutData$length>500 | allTroutData$length<0)
  badWeight<-which(allTroutData$weight>400 | allTroutData$weight<0)
  logLength<-log(allTroutData$length)
  logWeight<-log(allTroutData$weight)
  
  reg<-lm(logWeight~logLength,data=allTroutData)
  badLengthWeight<-which(reg$residuals>(mean(reg$residuals)+3*sd(reg$residuals))|
                           reg$residuals<(mean(reg$residuals)-3*sd(reg$residuals)))
  print(list('bad lengths',allTroutData[badLength],
             'bad weights',allTroutData[badWeight],
             'bad length-weight combos',allTroutData[badLengthWeight]))
  
  plot(weight~length,data=allTroutData,xlim=xlim,ylim=ylim)
  points(weight~length,data=allTroutData[badLength],pch="L")
  points(weight~length,data=allTroutData[badWeight],pch="W")
  points(exp(logWeight)~exp(logLength),data=reg$model[badLengthWeight,],pch="X",col='red')
}

checkPass<-function(){
  cat("passes all in 1:3",ifelse(all(allTroutData$pass %in% 1:3),"pass","fail"))
}
checkLatLong<-function(){
  cat("\nLatitudes reasonable",ifelse(
    all(allSiteData$siteLatitude<50 & allSiteData$siteLatitude>31),
    "pass","fail"))
  cat("\nLongitudes reasonable",ifelse(
    all(allSiteData$siteLongitude < -75 & allSiteData$siteLongitude > -90),
    "pass","fail"))
}

