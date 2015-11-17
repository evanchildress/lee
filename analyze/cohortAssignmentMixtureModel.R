library(mixtools)
library(R2jags)

load("~/lee/dataStore/cleanData/niles.RDATA")

#mixture.model<-normalmixEM(x=fish$length,lambda=0.5,mu=c(50,110,150),sigma=c(20,20,20))

muPriors<-matrix(c(40,60,
                   90,115,
                   115,150,
                   150,250),
                 nrow=4,byrow=T)
siteNum<-as.numeric(as.factor(fish$site))

jagsData<-list(length=fish$length,
               year=year(fish$date)-2010,
               nYears=length(unique(year(fish$date))),
               n=length(fish$length),
               nGroups=4,
               muPriors=muPriors,
               site=siteNum,
               nSites=length(unique(siteNum))
)

cat("model{


  for(g in 1:nGroups){
#overall mean length for each cohort
    mu[g]~dunif(muPriors[g,1],muPriors[g,2])

#variation on random year effect (mean effect only)
    sdYear[g]~dunif(0,20)
    tauYear[g]<-1/pow(sdYear[g],2)

#variation on random site effect (mean effect only)
    sdSite[g]~dunif(0,20)
    tauSite[g]<-1/pow(sdSite[g],2)

#within group/site/year variation (constant across site/year)
    sd[g]~dunif(0,100)
    tau[g]<-1/pow(sd[g],2)
  }

#random year effects
  for(y in 1:nYears){
    for(g in 1:nGroups){
      muY[g,y]~dnorm(mu[g],tauYear[g])T(muPriors[g,1],muPriors[g,2])

#fixed year effect on pu
      pu[g,y]~dunif(0,1)

#random site effect
      for(s in 1:nSites){
        muS[g,y,s]~dnorm(muY[g,y],tauSite[g])T(muPriors[g,1],muPriors[g,2])
      }
    }
  }

#likelihood
  for(i in 1:n){
    age[i]~dcat(pu[,year[i]])
    length[i]~dnorm(muS[age[i],year[i],site[i]],tau[age[i]])
  }

}",file='~/lee/analyze/model.txt')



nb=500
nt=3
ni=1000
nc=3

params<-c("age","muY","muS","sdYear","sdSite","pu","mu","sd")

out<-jags(jagsData,inits=NULL,params,"~/lee/analyze/model.txt",
          nc,ni,nb,nt)
saveRDS(out,"~/lee/results/cohortMixtureOut.rds")
sims<-out$BUGSoutput$sims.list
apply(sims$sd,2,mean)
apply(sims$mu,2,mean)
annualMeans<-apply(sims$muY,c(2,3),mean)
apply(sims$age,2,median)

fish[,age:=apply(sims$age,2,median)]
fish[age>1,age:=2]
save(fish,siteData,skippedPasses,file="leeBktWithAge.rDATA")
