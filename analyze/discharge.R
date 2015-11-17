library(waterData)
q<-data.table(importDVs("01552000",code="00060",stat="00003",sdate="1925-10-01",
                        edate=Sys.Date()))
setnames(q,c('val','dates'),c('discharge','date'))
q[discharge<0,discharge:=NA]

qRecurrence<-q[,max(discharge,na.rm=T),by=year(date)]
setnames(qRecurrence,"V1","qMax")
setkey(qRecurrence,qMax)
qRecurrence[,rank:=nrow(qRecurrence):1]
qRecurrence[,p:=rank/(nrow(qRecurrence)+1)]
qRecurrence[,ri:=(nrow(qRecurrence)+1)/rank]


plot(I((log10(discharge)-10)/2)~date,data=q,type='l')
