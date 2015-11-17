#see cut in jags
makeData<-function(lambda,p,nSites){
  n<-rpois(nSites,lambda)
  
  y<-array(NA,dim=c(nSites,3))
  
  for(i in 1:nSites){
    y[i,1]<-rbinom(1,n[i],p) #first pass
    y[i,2]<-rbinom(1,y[i,1],p) #second pass recaptures
    y[i,3]<-rbinom(1,n[i]-y[i,1],p) #second pass unmarked captures
  }

  return(list(n=n,p=p,y=y,lambda=lambda,nSites=nSites))
}

cat("model{
  #Priors
  lambda~dunif(0,1000)
  p~dunif(0,1)
  

  #Likelihood
  for(i in 1:nSites){
    #State process
    N[i]~dpois(lambda)
  
    #Observation process
    y[i,1]~dbin(p,N[i]) #first pass
    y[i,2]~dbin(p,y[i,1]) #second pass, recaptures
    y[i,3]~dbin(p,N[i]-y[i,1]) #second pass, unmarked individuals
  }

}",file="lincolnPetersenTestModel.txt")



inits<-function(){
  list(lambda=runif(0,1000),
       p=runif(0,1)
  )
}

params<-c("lambda","N","p")

ni=800
nt=2
nb=200
nc=3

run<-function(lambda,p,nSites){
  data<-makeData(lambda,p,nSites)
  win.data<-list(y=data$y,
                 nSites=data$nSites)
  
  out<-jags(win.data,inits=inits,params,"~/lee/lincolnPetersenTestModel.txt",n.chains=nc,n.iter=ni,
            n.thin=nt,n.burnin=nb,working.directory=getwd())
  sims<-out$BUGSoutput$sims.list
  getSummary<-function(param){
    Mean<-mean(sims[[param]])
    Quants<-quantile(sims[[param]],c(0.025,0.5,0.975))
    return(c(Mean,Quants))
  }
  
  result<-as.list(c(getSummary('lambda'),getSummary('p')))
  return(result)
}

nSims<-500
result<-data.table(lambdaMean=as.numeric(rep(NA,nSims)),
                   lambdaLwr=as.numeric(rep(NA,nSims)),
                   lambdaMedian=as.numeric(rep(NA,nSims)),
                   lambdaUpr=as.numeric(rep(NA,nSims)),
                   pMean=as.numeric(rep(NA,nSims)),
                   pLwr=as.numeric(rep(NA,nSims)),
                   pMedian=as.numeric(rep(NA,nSims)),
                   pUpr=as.numeric(rep(NA,nSims)))

for(i in 1:nSims){
  result[i]<-run(300,0.6,100)
  print(i)
}

saveRDS(result,"lincolnPetersenSimulationResults.rds")
