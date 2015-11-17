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
data[,recapCount:=as.integer(recapCount)] #starts as character, spits warning b/c coerces "NULL" to NA

data[species=="Brook Trout",species:="bkt"]
data[species=="Brown Trout",species:="bnt"]

data<-data[!duplicated(data)]

assign('data',data,env=shared_data)