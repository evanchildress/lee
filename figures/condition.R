load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")
fish<-fish[,siteNum:=as.numeric(as.factor(site))]
fish<-fish[!is.na(length)&!is.na(weight)&species=="brookTrout"]

fish<-fish[,condition:=100000*weight/length^3]
fish<-fish[,age:=ifelse(age==1,1,2)]

condition<-fish[,.(condition=mean(condition),n=.N),.(site,year(date),age)] %>%
  setkey(year) %>%
  .[,.SD[J(2011:2015)],by=list(site,age)] %>%
  .[is.na(n),n:=0] %>%
  .[,allN:=all(n>0),by=list(site,age)] %>%
  .[allN==T] %>%
  select(-allN) %>%
  setkey(site,age,year)

sims<-readRDS("~/lee/results/conditionFixedYear.rds")$BUGSoutput$sims.list

getSummary<-function(x){
  out<-c(mean(x),quantile(x,c(0.025,0.975)))
  names(out)<-NULL
  return(out)
}

mu<-apply(sims$mu,c(2,3),getSummary)
mu<-melt(mu) %>%
  dcast(Var2+Var3~Var1) %>%
  data.table() %>%
  setnames(c("year","age","mean","lower","upper")) %>%
  mutate(year=year+2010)

tiff.par("~/lee/figures/condition.tif",width=6.8,height=3.6,
         mar=c(1.5,2.5,1,0),mfrow=c(1,2),mgp=c(1.7,0.5,0))
for(a in 1:2){
  sites<-unique(condition[age==a,site])
  plot(condition~year,data=condition,pch=NA,bty='l',
       ylim=c(0.49,1.18),ylab="Condition Factor",xlab="",
       main=c("YOY","Adult")[a])
  for(s in sites){
    points(condition~year,
           data=condition[age==a&site==s],
           type='l',col='gray')
  }
  points(mean~year,data=mu[age==a],type='l',lwd=2)
  with(mu[age==a],error.bar(year,mean,upper,lower,interval.type="absolute"))
  points(mean~year,data=mu[age==a],
         pch=list(c(19,15,17,17,15),
                  c(19,19,15,17,17))[[a]],
         col=gray(0.01,alpha=0.5),cex=1.2)
  points(mean~year,data=mu[age==a],
         pch=list(NA,
                  c(NA,17,NA,NA,NA))[[a]],
         col=gray(0.01,alpha=0.5),cex=1.2)
  #   if(a==2){
  #       points(mean~year,data=mu[age==a],
  #          pch=list(c(19,15,17,17,15),
  #                   c(19,19,15,17,17)))
  #   }

}
dev.off()