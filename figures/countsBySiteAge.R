library(reshape2)

counts<-fish[species=="brookTrout",.(n=length(length)),by=list(site,date,age,species)]
counts[,year:=year(date)]
counts[,date:=NULL]
counts[,species:=NULL]

counts<-dcast.data.table(melt(counts,id.vars=c("site","year","age")),
                 site~year+age)
counts[is.na(counts)]<-as.integer(0)

counts<-melt(counts,id.vars="site")
counts[,year:=as.numeric(substring(variable,1,4))]
counts[,age:=as.numeric(substring(variable,6,6))]
counts[,variable:=NULL]
setnames(counts,"value","n")

colors<-sample(colors(),length(unique(counts$site)))
tiff.par("~/lee/figures/countsBySiteAge.tif",
         mfrow=c(1,2))
plot(n~year,data=counts,pch=NA)
for(i in unique(counts$site)){
  points(n~year,
         data=counts[site==i&age==1],
         type='l',lwd=2,col=colors[which(i==unique(counts$site))])
}

plot(n~year,data=counts,pch=NA)
for(i in unique(counts$site)){
  points(n~year,
         data=counts[site==i&age==2],
         type='l',lwd=2,col=colors[which(i==unique(counts$site))])
}
dev.off()