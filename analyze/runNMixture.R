library(reshape2)
source("~/lee/analyze/nMixtureModel.R")
load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")
counts<-fish[species=="brookTrout",.(n=length(length)),by=list(site,date,age,pass,species)]
counts[,year:=year(date)]
counts[,date:=NULL]
counts[,species:=NULL]

counts<-dcast.data.table(melt(counts,id.vars=c("site","year","age","pass")),
                         site~year+age+pass)
counts[is.na(counts)]<-as.integer(0)

counts<-melt(counts,id.vars="site")
counts[,year:=as.numeric(substring(variable,1,4))]
counts[,age:=as.numeric(substring(variable,6,6))]
counts[,pass:=as.numeric(substring(variable,8,8))]
counts[,variable:=NULL]
setnames(counts,"value","n")

skippedPasses[,year:=year(date)]
setkey(counts,site,year,pass)
setkey(skippedPasses,site,year,pass)
counts[skippedPasses,n:=NA]

countArray<-acast(melt(counts,id.vars=c("site","year","age","pass")),
              site~year~age~pass)

siteWidth<-acast(melt(siteData[,list(site,"year"=year(date),siteAvgWidth)],
                      id.vars=c("site","year")),
                 site~year)
widthNas<-which(is.na(siteWidth))
for(i in widthNas){
  siteWidth[i]<-rowMeans(siteWidth,na.rm=T)[
                  min((i-30*0:4)[which(i-30*0:4>0)])]
}
siteWidthCentered<-(siteWidth-mean(siteWidth))/sd(siteWidth)


jagsData<-list(y=countArray,
               nSites=dim(countArray)[1],
               nYears=dim(countArray)[2],
               siteWidth=siteWidthCentered)

nInits<-apply(countArray,c(1,2,3),sum,na.rm=T)

inits<-function(){list(N=nInits)}

params<-c("lambda","N","sdSite","sdYear","beta")

ni=3000
nb=2000
nt=2
nc=3

out<-jags(jagsData,inits=inits,params,"model.txt",nc,ni,nb,nt)
saveRDS(out,"~/lee/results/nMixOut.rds")
