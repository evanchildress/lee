sims<-readRDS("~/lee/results/nMixture.rds")$BUGSoutput$sims.list
load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")

siteWidth<-acast(melt(siteData[,list(site,"year"=year(date),
                                     siteAvgWidth)],
                      id.vars=c("site","year")),
                 site~year)
widthNas<-which(is.na(siteWidth))
for(i in widthNas){
  siteWidth[i]<-rowMeans(siteWidth,na.rm=T)[
    min((i-30*0:4)[which(i-30*0:4>0)])]
}
siteWidthCentered<-(siteWidth-mean(siteWidth))/sd(siteWidth)

betas<-apply(sims$betaP,c(2,3),mean)
x<-seq(-2,5,0.1)
meanP<-array(NA,dim=c(length(x),2))
for(k in 1:2){
  meanP[,k]<-inv.logit(betas[1,k]+x*betas[2,k])
}

tiff.par('~/lee/figures/detection.tif')

xStd<-seq(-2,5,0.1)
x<-xStd*sd(siteWidth)+mean(siteWidth)
p<-array(NA,dim=c(length(x),2))

par(mar=c(2.5,3,1,0),mgp=c(1.5,0.5,0),las=1)
layout(matrix(c(2,1)),widths=5,heights=c(5,2))

hist(siteWidth,breaks=seq(0,7.5,0.25),xlab="Site Width (m)",
     main="")

plot(NA,ylim=c(0,1),xlim=c(0,7.5),type='l',lwd=2,lty=2,
     xlab="Site Width (m)",ylab="")

for(i in 1:dim(sims$betaP)[1]){
  for(k in 1:2){
    p<-1/(1+exp(-(sims$betaP[i,1,k]+xStd*sims$betaP[i,2,k])))
    points(p~x,col=c('gray','black')[k],
           type='l')
  }
}
title(ylab="Detection Probability",line=1.9)
legend(6,1,c("YOY","Adults"),lty=1,lwd=1,
       col=c('gray','black'),bty='n')


# points(meanP[,1]~I(x*sd(siteWidth)+mean(siteWidth)),
#      ylim=c(0,1),type='l',lwd=2,col='gray',
#      xlab="Site Width (m)",ylab="")
# title(ylab="Detection Probability",line=1.9)
# points(meanP[,2]~I(x*sd(siteWidth)+mean(siteWidth)),
#        type='l',lwd=2)
# legend(6,0.9,c("YOY","Adults"),lty=c(2,1),lwd=2,bty='n')
dev.off()
#detection probabilities at average site width
getSummary<-function(x){
  out<-c(mean(x),quantile(x,c(0.025,0.975)))
  names(out)<-NULL
  return(out)
}
betas<-apply(sims$betaP,c(2,3),getSummary)
p<-1/(1+exp(-(betas[,1,])))
