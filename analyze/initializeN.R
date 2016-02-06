load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")
fish[age>=2,age:=2]
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
counts<-acast(melt(counts,id.vars=c("site","year","age","pass")),
                  site~year~age~pass)

nInits<-apply(counts,c(1,2,3),sum,na.rm=T)+20
sInits<-nInits[,1:4,]
nInits[,2:5,2]<-NA 

inits<-function(){list(N=nInits
                       ,S=sInits
)}

