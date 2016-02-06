##Load data and packages
library(R2jags)
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)


source("~/lee/figures/densityBiomass.R")
source("~/lee/analyze/waicFunction.R")

load("~/lee/dataStore/cleanData/leeBktWithAge.rdata")
fish<-fish[,siteNum:=as.numeric(as.factor(site))]
fish<-fish[!is.na(length)&species=="brookTrout"]
fish<-fish[,age:=ifelse(age==1,1,2)]
scaledDensity<-density
scaledDensity[,,1]<-array(scale(melt(density[,,1])$value)[,1],dim=c(30,5))
scaledDensity[,,2]<-array(scale(melt(density[,,2])$value)[,1],dim=c(30,5))

loggedDensity<-log(density)
scaledLoggedDensity<-density
scaledLoggedDensity[,,1]<-array(scale(melt(loggedDensity[,,1])$value)[,1],dim=c(30,5))
scaledLoggedDensity[,,2]<-array(scale(melt(loggedDensity[,,2])$value)[,1],dim=c(30,5))

waicOut<-list()
for(d in c("density","scaledDensity","loggedDensity","scaledLoggedDensity")){
###Model length with fixed year effect
jagsData<-list(length=fish$length,
               year=year(fish$date)-2010,
               nYears=length(unique(year(fish$date))),
               site=fish$siteNum,
               n=nrow(fish),
               nSites=length(unique(fish$siteNum)),
               age=fish$age,
               density=get(d)
)

cat("model{
    #fixed year effect
#     for(y in 1:nYears){
#     for(a in 1:2){
#     mu[y,a]~dnorm(0,0.0001)
#     }
#     }
    #within site variation
    for(a in 1:2){
    sigma[a]~dunif(0,100)
    tau[a]<-1/pow(sigma[a],2)
    }
    for(a in 1:2){
    #random site effect
    sigmaSite[a]~dunif(0,100)
    tauSite[a]<-1/pow(sigmaSite[a],2)
    
    for(s in 1:nSites){
    muS[s,a]~dnorm(0,tauSite[a])
    }
    #betas for density effect
    for(b in 1:2){
    beta[b,a]~dnorm(0,0.01)
    }
    }
    
    
    for(i in 1:n){
    muSY[i]<-muS[site[i],age[i]]+
    beta[1,age[i]]*density[site[i],year[i],1]+
    beta[2,age[i]]*density[site[i],year[i],2]
    
    sigmaX[i]<-sigma[age[i]]
    length[i]~dnorm(muSY[i],tau[age[i]])
    }
    }",file="~/lee/analyze/model.txt")

ni=4000
nb=3000
nt=2
nc=3

inits<-function(){list(mu=array(c(rep(55,5),rep(120,5)),dim=c(5,2)))
}

params<-c("sigmaSite","sigma","muS","beta","muSY","sigmaX")


out<-jags(jagsData,inits=inits,params,"~/lee/analyze/model.txt",nc,ni,nb,nt)

sims<-out$BUGSoutput$sims.list
assign(paste0(d,"Beta"),apply(sims$beta,c(2,3),getSummary))

waicOut[[d]]<-waic(out,fish$length,type="linearModel",
            paramNames=list(mu="muSY",sigma="sigmaX"))
}