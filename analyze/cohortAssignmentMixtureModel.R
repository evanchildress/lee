library(mixtools)
library(R2jags)

load("~/lee/dataStore/cleanData/niles.RDATA")

#mixture.model<-normalmixEM(x=fish$length,lambda=0.5,mu=c(50,110,150),sigma=c(20,20,20))

muPriors<-matrix(c(50,35,70,
                   100,90,115,
                   135,115,150,
                   175,150,250),
                 nrow=4,byrow=T)
siteNum<-as.numeric(as.factor(fish$site))

jagsigmaata<-list(length=fish$length,
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
    mu[g]~dnorm(muPriors[g,1],0.001)T(muPriors[g,2],muPriors[g,3])

#variation on random year effect (mean effect only)
#     tauYear[g]~dgamma(0.001,0.001)
#     sigmaYear[g]<-1/sqrt(tauYear[g])
    tauYear[g]<-1/pow(sigmaYear[g],2)
    sigmaYear[g]~dunif(0,20)

# #variation on random site effect (mean effect only)
#     sigmaSite[g]~dunif(0,20)
#     tauSite[g]<-1/pow(sigmaSite[g],2)

#within group/site/year variation (constant across site/year)
#     tau[g]~dgamma(0.001,0.001)
#     sigma[g]<-1/sqrt(tau[g])
    sigma[g]~dunif(0,50)
    tau[g]<-1/pow(sigma[g],2)
  }

#random year effects
  for(y in 1:nYears){
    for(g in 1:nGroups){
      muY[g,y]~dnorm(mu[g],tauYear[g])T(muPriors[g,2],muPriors[g,3])

#fixed year effect on pu
      pu[g,y]~dunif(0,1)


      for(s in 1:nSites){
#         muS[g,y,s]~dnorm(muY[g,y],tauSite[g])T(muPriors[g,2],muPriors[g,3]) #random site effect
        muS[g,y,s]<-muY[g,y]
      }
    }
  }

#likelihood
  for(i in 1:n){
    age[i]~dcat(pu[,year[i]])
    length[i]~dnorm(muS[age[i],year[i],site[i]],tau[age[i]])
  }

}",file='~/lee/analyze/model.txt')



nb=1000
nt=3
ni=2000
nc=3

params<-c("muY","muS","sigmaYear","sigmaSite","pu","mu","sigma")

out<-jags(jagsigmaata,inits=NULL,params,"~/lee/analyze/model.txt",
          nc,ni,nb,nt)
saveRDS(out,"~/lee/results/cohortMixtureOut.rds")
sims<-out$BUGSoutput$sims.list
apply(sims$sigma,2,mean)
apply(sims$mu,2,mean)
annualMeans<-apply(sims$muY,c(2,3),mean)
#apply(sims$age,2,median)

fish[,age:=apply(sims$age,2,median)]
save(fish,siteData,skippedPasses,file="~/lee/dataStore/cleanData/leeBktWithAge.rDATA")
