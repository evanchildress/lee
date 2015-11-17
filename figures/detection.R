sims<-readRDS("~/lee/results/dailMadsenOut.rds")$BUGSoutput$sims.list
load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")

siteWidth<-acast(melt(siteData[,list(site,"year"=year(date),siteAvgWidth)],
                      id.vars=c("site","year")),
                 site~year)
widthNas<-which(is.na(siteWidth))
for(i in widthNas){
  siteWidth[i]<-rowMeans(siteWidth,na.rm=T)[
    min((i-30*0:4)[which(i-30*0:4>0)])]
}
siteWidthCentered<-(siteWidth-mean(siteWidth))/sd(siteWidth)

betas<-apply(sims$beta,c(2,3),mean)
x<-seq(-2,5,0.1)
p<-array(NA,dim=c(length(x),2))
for(k in 1:2){
  p[,k]<-inv.logit(betas[1,k]+x*betas[2,k])
}

tiff.par('~/lee/figures/detection.tif',mar=c(2.5,2.9,0,0))
  plot(p[,1]~I(x*sd(siteWidth)+mean(siteWidth)),
      ylim=c(0,1),type='l',lwd=2,lty=2,
      xlab="Site Width (m)",ylab="")
  title(ylab="Detection Probability",line=1.9)
  points(p[,2]~I(x*sd(siteWidth)+mean(siteWidth)),
       type='l',lwd=2)
  legend(6,0.9,c("YOY","Adults"),lty=c(2,1),lwd=2,bty='n')
#abline(v=siteWidth)
dev.off()