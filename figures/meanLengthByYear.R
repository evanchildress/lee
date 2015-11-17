library(vioplot)
sims<-readRDS("~/lee/results/cohortMixtureOut.rds")$BUGSoutput$sims.list
load("~/lee/dataStore/cleanData/niles.rDATA")
apply(sims$sd,2,mean)
apply(sims$mu,2,mean)
annualMeans<-apply(sims$muY,c(2,3),mean)
apply(sims$age,2,median)

fish[,age:=apply(sims$age,2,median)]
fish[age>=2,age:=2]

# violinPlots<-function(x,by=NA,data=NULL){
#   byCombos<-unique(data[,by])
#   vioplot(x[which]
# }

tiff.par("~/lee/figures/lengthViolinPlots.tif",mfcol=c(2,5),
         mar=c(2.5,2.5,1,0),width=3,height=5,oma=c(0,0,1,0))
  fish[!is.na(length),vioplot(length,ylim=c(30,300)),list(year(date),age)]
dev.off()  

tiff.par("~/lee/figures/lengthBoxPlots.tif",
         mar=c(2.5,2.5,1,0),width=3,height=5,oma=c(0,0,1,0))
  boxplot(length~year(date)+age,data=fish,names=rep(2011:2015,2))
dev.off()  


