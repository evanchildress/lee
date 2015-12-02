sims<-readRDS("~/lee/results/cohortMixtureOutBkt.rds")$BUGSoutput$sims.list
load("~/lee/dataStore/cleanData/niles.rDATA")

fish<-fish[species=='brookTrout']
fish[,age:=apply(sims$age,2,median)]
fish[,age:=sims$age[600,]]
makeHist<-function(y,ylimit=T){
  for(a in 1:4){
    assign(paste0("hist",a),
           hist(fish[age==a&year(date)==y,length],breaks=seq(0,320,1),plot=F))
  }
  counts<-as.matrix(data.frame(c1=hist1$counts,c2=hist2$counts,c3=hist3$counts,c4=hist4$counts))
  if(ylimit){ylimit<-c(0,120)
  } else {ylimit<-c(0,max(counts)*1.1)}
  barplot(t(counts),
          col=c('red','black','blue','gray'),
          main=y,space=0,bty='l',border=NA,
          ylim=ylimit)
  axis(1,seq(0,300,50)-0.5,seq(0,300,50))
}

tiff.par("~/lee/figures/lengthHistByYear.tif",mfrow=c(5,1),
         mar=c(2.5,2.5,1,0),width=3,height=5,oma=c(0,0,1,0),lwd=1)
for(y in 2011:2015){
  makeHist(y,ylimit=F)
  if(y==2011){
    legend("top",c("age0","age1","age2","age3+"),
            bty='n',inset=-0.65,horiz=T,xpd=NA,x.intersp=0.0,adj=0.5,
           text.col=c('red','black','blue','gray'))
    }
  if(y==2013){title(ylab="Frequency",cex=1.5)}
}
title(xlab="Length (mm)",cex=1.5)
dev.off()  

tiff.par("~/lee/figures/lengthHistByYear2.tif",mfrow=c(5,1),
         mar=c(2.5,2.5,1,0),width=3,height=5,oma=c(0,0,1,0))
for(y in 2011:2015){
  makeHist(y,ylimit=T)
  if(y==2011){
    legend("top",c("age0","age1","age2","age3+"),
           bty='n',inset=-0.65,horiz=T,xpd=NA,x.intersp=0.0,adj=0.5,
           text.col=c('red','black','blue','gray'))
  }
  if(y==2013){title(ylab="Frequency",cex=1.5)}
}
title(xlab="Length (mm)",cex=1.5)
dev.off()  