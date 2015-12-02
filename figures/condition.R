load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")

fish<-fish[!is.na(length)&!is.na(weight)]

fish[,logWeight:=log(weight)]
fish[,logLength:=log(length)]

lengthWeight<-lm(logWeight~logLength,data=fish)

fish[,condition:=100000*weight/length^3]
#fish[,condition:=residuals(lengthWeight)]
condition<-fish[,list(mean(condition,na.rm=T),
                      length(condition)),by=list(site,year(date),age)]
setnames(condition,c("V1","V2"),c("condition","n"))

colors<-sample(colors(),100)

tiff.par("~/lee/figures/condition.tif",mfrow=c(2,1))

plot(condition~year,data=condition,pch=NA)
for(i in unique(condition$site)){
  color<-colors[which(i==unique(condition$site))]
  points(condition~year,data=condition[site==i&age==1],type='l',lwd=2,
         col=color)
}

plot(condition~year,data=condition,pch=NA)
for(i in unique(condition$site)){
  color<-colors[which(i==unique(condition$site))]
  points(condition~year,data=condition[site==i&age==2],type='l',lwd=2,
         col=color)
}
dev.off()