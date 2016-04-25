data<-fread("qrystbtpopest.csv")
data[,date:=as.Date(SurveyDate,format="%m/%d/%Y")]
data[,year:=year(date)]
data[,month:=month(date)]

setnames(data,
         c("WaterSectionID","WaterSiteSurvey_ID","SurveySiteLatDD","SurveySiteLonDD","SiteLength_m",
                "SiteWidth_m","Comname","GroupSize","EffortCatch","DeadOrMarkedCapture","EffortNumber",
           "EstimateType"),
         c("siteId","siteSurveyId","lat","long","siteLength","siteWidth",
           "species","sizeBin","count","recapCount","pass","estimateType"))
data<-data[,list(siteId,siteSurveyId,lat,long,siteLength,siteWidth,species,sizeBin,count,recapCount,
                 pass,estimateType,date,month,year)]

bktBins<-data[species=='Brook Trout',unique(sizeBin)]



siteSurveyData<-data[,list(siteId,siteSurveyId,lat,long,siteLength,siteWidth,species,sizeBin,
                           estimateType,date,month,year)]
siteSurveyData<-siteSurveyData[!duplicated(siteSurveyData)]
setkey(siteSurveyData,siteSurveyId,species,sizeBin)

markRecap<-data[estimateType=="Petersen M & R",list(siteSurveyId,species,sizeBin,count,recapCount,pass)]
markRecap[recapCount=="NULL",recapCount:=NA]
markRecap[,recapCount:=as.integer(recapCount)]
markRecap<-markRecap[!duplicated(markRecap)]



mr<-dcast.data.table(melt(markRecap[,list(siteSurveyId,species,sizeBin,pass,count)],
               c("siteSurveyId","species","sizeBin","pass")),siteSurveyId+species+sizeBin~pass)
setnames(mr,c('1','2'),c('pass1','pass2'))
setkey(mr,siteSurveyId,species,sizeBin)

recaps<-markRecap[pass==2,list(siteSurveyId,species,sizeBin,recapCount)]
setkey(recaps,siteSurveyId,species,sizeBin)

mr<-mr[recaps]
mr[,nEst:=(pass1+1)*(pass2+1)/(recapCount+1)]
mr<-mr[,list(siteSurveyId,species,sizeBin,nEst)]
setkey(mr,siteSurveyId,species,sizeBin)

mr<-siteSurveyData[mr]

lp<-function(){
  data<-markRecap[siteSurveyId==site&year==yr&sizeBin==size]
  nEst<-(data[pass==1,count]*data[pass==2,count])/data[pass==2,recapCount]
  return(nEst)
}
