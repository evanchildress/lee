sims<-readRDS("~/lee/results/dailMadsenOut.rds")$BUGSoutput$sims.list
nOut<-apply(sims$N,c(2,3,4),mean)
nOut<-data.table(melt(nOut))
setnames(nOut,c("site","year","age","n"))
nOut[,year:=year+2010]


colors<-sample(colors(),100)
tiff.par("~/lee/figures/estimatedN.tif",mfrow=c(2,1))
plot(n~year,data=nOut[age==1],pch=NA)
for(i in 1:length(unique(nOut$site))){
  points(n~year,data=nOut[age==1&site==i],type='l',col=colors[i])
}


plot(n~year,data=nOut[age==2],pch=NA)
for(i in 1:length(unique(nOut$site))){
  points(n~year,data=nOut[age==2&site==i],type='l',col=colors[i])
}

dev.off()