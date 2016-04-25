sims<-readRDS("~/lee/results/cohortMixtureOut.rds")$BUGSoutput$sims.list
load("~/lee/dataStore/cleanData/niles.rDATA")
apply(sims$sd,2,mean)
apply(sims$mu,2,mean)
annualMeans<-apply(sims$muY,c(2,3),mean)
apply(sims$age,2,median)

fish[,age:=apply(sims$age,2,median)]
fish[age>=2,age:=2]
fish[,logLength:=log(length)]
fish[,logWeight:=log(weight)]

meanWidth<-siteData[,mean(siteAvgWidth,na.rm=T),list(site)]
setnames(meanWidth,"V1","meanWidth")
setkey(meanWidth,site)
setkey(fish,site)
fish<-meanWidth[fish]

weightLm<-lm(logWeight~logLength,data=fish)
fish[is.na(weight),weight:=predict(weightLm,data.frame(logLength=logLength))]
#still 4 fish with na length and weight

biomass<-fish[,list(sum(weight,na.rm=T),mean(meanWidth)),by=list(site,year(date))]
setnames(biomass,c("V1","V2"),c("biomass","siteWidth"))

colors<-sample(colors(),100)

tiff.par("~/lee/figures/biomass.tif")
plot(biomass~year,data=biomass,pch=NA)
for(i in unique(biomass$site)){
  color<-colors[which(i==unique(biomass$site))]
  points(biomass~year,data=biomass[site==i],type='l',lwd=2,
         col=color)
}
dev.off()

biomass[,biomassProp:=biomass/mean(biomass),by=site]
biomass[,meanSiteBiomass:=mean(biomass),by=site]
biomass[,weighting:=meanSiteBiomass/sum(meanSiteBiomass),by=year]
weightedMeanBiomassProp<-biomass[,weighted.mean(biomassProp,weighting),by=year]
meanBiomassProp<-biomass[,mean(biomassProp),by=year]

colfunc<-colorRampPalette(c('yellow','blue'))
colors<-colfunc(max(biomass[,mean(biomass),by=site]$V1))

tiff.par("~/lee/figures/biomassProp.tif")
plot(biomassProp~year,data=biomass,pch=NA)
for(i in unique(biomass$site)){
  color<-colors[mean(biomass[site==i,biomass])]
  points(biomassProp~year,data=biomass[site==i],type='l',lwd=2,
         col=color)
  points(V1~year,data=weightedMeanBiomassProp,col='black',
         type='l',lwd=4,lty=2)
  points(V1~year,data=meanBiomassProp,col='black',
         lwd=4,type='l')
}
dev.off()

biomass$scaledBiomass<-biomass$biomass/biomass$siteWidth

colors<-colfunc(round(max(biomass$siteWidth)))

tiff.par("~/lee/figures/biomassScaled.tif")
plot(scaledBiomass~year,data=biomass,pch=NA)
for(i in unique(biomass$site)){
  color<-colors[biomass[site==i,mean(siteWidth)]]
  points(scaledBiomass~year,data=biomass[site==i],type='l',lwd=2,
         col=color)
}
dev.off()


#cv of biomass vs cv of abundance

biomassCv<-biomass[,sd(biomass)/mean(biomass),by=site]
setnames(biomassCv,"V1","biomassCv")

totalN<-nOut[,sum(n),by=list(site,year)]
nCv<-totalN[,sd(V1)/mean(V1),by=site]

nCvAge<-nOut[,sd(n)/mean(n),by=list(site,age)]


setnames(nCv,"V1","nCv")
tiff.par("~/lee/figures/cvHistograms.tif",mfrow=c(2,2))
hist(biomassCv$biomassCv,breaks=seq(0,2.5,0.1))
hist(nCv$nCv,breaks=seq(0,2.5,0.1))
hist(nCvAge[age==1,V1],breaks=seq(0,2.5,0.1))
hist(nCvAge[age==2,V1],breaks=seq(0,2.5,0.1))
dev.off()

