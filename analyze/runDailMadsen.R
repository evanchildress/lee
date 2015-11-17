library(reshape2)

#make the model file
source("~/lee/analyze/constructDailMadsen.R")

#load data and eliminate extraneous columns
load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")
counts<-fish[species=="brookTrout",.(n=length(length)),by=list(site,date,age,pass,species)]
counts[,year:=year(date)]
counts[,date:=NULL]
counts[,species:=NULL]

#zero counts are missing in data, so reshape and fill NAs with 0s
counts<-dcast.data.table(melt(counts,id.vars=c("site","year","age","pass")),
                         site~year+age+pass)
counts[is.na(counts)]<-as.integer(0)

counts<-melt(counts,id.vars="site")
counts[,year:=as.numeric(substring(variable,1,4))]
counts[,age:=as.numeric(substring(variable,6,6))]
counts[,pass:=as.numeric(substring(variable,8,8))]
counts[,variable:=NULL]
setnames(counts,"value","n")

#some NAs are true skipped passes, so return those to NA
skippedPasses[,year:=year(date)]
setkey(counts,site,year,pass)
setkey(skippedPasses,site,year,pass)
counts[skippedPasses,n:=NA]

#make the counts an array
countArray<-acast(melt(counts,id.vars=c("site","year","age","pass")),
              site~year~age~pass)

#prepare siteWidth detection covariate
siteWidth<-acast(melt(siteData[,list(site,"year"=year(date),siteAvgWidth)],
                      id.vars=c("site","year")),
                 site~year)
  #fill width NAs with mean for that site
  widthNas<-which(is.na(siteWidth))
  for(i in widthNas){
    siteWidth[i]<-rowMeans(siteWidth,na.rm=T)[
                    min((i-30*0:4)[which(i-30*0:4>0)])]
  }
siteWidthCentered<-(siteWidth-mean(siteWidth))/sd(siteWidth)

#data structure
nAges<-2; nYears<-dim(countArray)[2]; nSites<-dim(countArray)[1]

jagsData<-list(y=countArray,
               nSites=nSites,
               nYears=nYears,
               nAges=nAges,
               siteWidth=siteWidthCentered)

nInits<-apply(countArray,c(1,2,3),sum,na.rm=T)+20
sInits<-nInits[,1:4,]
nInits[,2:5,2]<-NA 

inits<-function(){list(N=nInits
                       ,S=sInits
                       )}

params<-c("lambdaMu","lambdaSigma","phiMu","phiSigma","alphaSigma","alphaMu","beta")

ni=3000
nb=2000
nt=2
nc=3

out<-jags(jagsData,inits=inits,params,"model.txt",nc,ni,nb,nt)
saveRDS(out,"~/lee/results/dailMadsenOut.rds")

sims<-out$BUGSoutput$sims.list
phi<-apply(sims$phiMu,c(2,3),
           function(x){inv.logit(mean(x))})
alpha<-apply(sims$alphaMu,2,
           function(x){exp(mean(x))})
lambda<-apply(sims$lambdaMu,2,
              function(x){exp(mean(x))})

