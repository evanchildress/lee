#library(mixtools)
library(R2jags)
library(data.table)

load("~/lee/dataStore/cleanData/niles.rDATA")
bkt<-fish[species=="brookTrout"]

#mixture.model<-normalmixEM(x=fish$length,lambda=0.5,mu=c(50,110,150),sigma=c(20,20,20))
nYears<-length(unique(year(bkt$date)))
nGroups<-4

muPriors<-matrix(c(50,35,70,
                   100,90,125,
                   150,125,175,
                   200,175,300),
                 nrow=4,byrow=T)
siteNum<-as.numeric(as.factor(bkt$site))

jagsdata<-list(length=bkt$length,
               year=year(bkt$date)-2010,
               nYears=nYears,
               n=length(bkt$length),
               nGroups=nGroups,
               muPriors=muPriors,
               site=siteNum,
               nSites=length(unique(siteNum))
)

cat("model{
    
    
    for(g in 1:nGroups){
    for(y in 1:nYears){ #fixed year effect
    #overall mean length for each cohort in each year
    mu[g,y]~dnorm(0,0.001)I(muPriors[g,2],muPriors[g,3])
    
    #fixed year effect on pu (inclusion in group g)
    beta[g,y]~dgamma(1,1)
    pu[g,y]<-beta[g,y]/sum(beta[,y])
    }
    #variation on random year effect (mean effect only)
    #     tauYear[g]~dgamma(0.001,0.001)
    #     sigmaYear[g]<-1/sqrt(tauYear[g])
    #     tauYear[g]<-1/pow(sigmaYear[g],2)
    #     sigmaYear[g]~dunif(0,20)
    
    # #variation on random site effect (mean effect only)
    #     sigmaSite[g]~dunif(0,20)
    #     tauSite[g]<-1/pow(sigmaSite[g],2)
    
    #within group/site/year variation (constant across site/year)
    #       tau[g]~dgamma(0.001,0.001)
    #       sigma[g]<-1/sqrt(tau[g])
    sigma[g]~dunif(0,16)
    tau[g]<-1/pow(sigma[g],2)
    }
    
    #random year effects
    for(y in 1:nYears){
    for(g in 1:nGroups){
    #       muY[g,y]~dnorm(mu[g],tauYear[g])T(muPriors[g,2],muPriors[g,3])
    
    
    
    
    for(s in 1:nSites){
    #           muS[g,y,s]~dnorm(muY[g,y],tauSite[g])T(muPriors[g,2],muPriors[g,3]) #random site effect
    muS[g,y,s]<-mu[g,y]
    }
    }
    }
    
    #likelihood
    for(i in 1:n){
    age[i]~dcat(pu[,year[i]])
    length[i]~dnorm(muS[age[i],year[i],site[i]],tau[age[i]])
    }
    
    }",file='~/lee/model.bugs')



nb=1000
nt=5
ni=3000
nc=3

params<-c("pu","mu","sigma","age")

out<-bugs(jagsdata,inits=NULL,params,"~/lee/model.bugs",
          n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,debug=T)
saveRDS(out,"~/lee/results/cohortMixtureOutBkt.rds")
sims<-out$BUGSoutput$sims.list
# apply(sims$sigma,2,mean)
# apply(sims$mu,2,mean)
# annualMeans<-apply(sims$muY,c(2,3),mean)
#apply(sims$age,2,median)

bkt[,age:=round(apply(sims$age,2,median))]
#bkt[,age:=sims$age[600,]]
fish<-bkt
save(fish,siteData,skippedPasses,file="~/lee/dataStore/cleanData/leeBktWithAge.rDATA")

