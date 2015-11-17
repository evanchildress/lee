cat(
"model{
  #Priors
  
  #initial abundance parameters
  for(a in 1:3){
    lambdaMu[a]~dnorm(0,0.01) #average abundance for starting year
    lambdaEps[a]~dunif(0,5) #variance of sites for starting year
    
    for(s in 1:nSites){
      logLambda[s,a]~dnorm(lambdaMu[a],lambdaEps[a]) #random site effect for initial abundance
      lambda[s,a]<-exp(logLambda[s,a])
    }
  }
  
  #survival parameters
  for(t in 1:(nYears-1)){
    for(a in 1:3){ 
      phiMu[t,a]~dunif(0,1) #average phi for each time/stage
      
      for(s in 1:nSites){
        phi[s,t,a]<-phiMu[t,a] #constant across sites, should probably be random
      }
    }
  }
  
  #per capita reproduction
  for(t in 1:(nYears-1)){
    alphaMu[t]~dunif(0,50) #varies across time
    for(s in 1:nSites){
      alpha[s,t]<-alphaMu[t] #just using a yearly value here, should be random by site probably
    }
  }
  
  pMu~dunif(0,1) #constant detection probability across sites and time and stages
  for(s in 1:nSites){
    for(t in 1:nYears){
      for(a in 1:3){
        p[s,t,a]<-pMu
      }
    }
  }
  
  #Likelihood
  
  #State process
  #establish initial abundance for each stage
  for(s in 1:nSites){
    for(a in 1:3){
      N[s,1,a]~dpois(lambda[s,a])
    }
  }
  
  #loop through subsequent years
  for(s in 1:nSites){
    for(t in 1:(nYears-1)){
      #stages: 1=yoy, 2=1-year old, 3=2+ year old
      #yoy abundance depends on number of adults and a per capita spawning parameter
      N[s,t+1,1]~dpois((N[s,t,2]+N[s,t,3])*alpha[s,t])
      
      #1 year old abundance depends on survival of last year's yoy
      N[s,t+1,2]~dbin(phi[s,t,1],N[s,t,1])
      
      #2+ in t+1 split into maturing 1 year olds and surviving 2+
      N2[s,t+1,1]~dbin(phi[s,t,2],N[s,t,2]) #survival of 1 year olds
      N2[s,t+1,2]~dbin(phi[s,t,3],N[s,t,3]) #survival of 2+ year olds
      N[s,t+1,3]<-N2[s,t+1,1]+N2[s,t+1,2] #summed for total 2+ in t+1
    }
  }
  
  #Observation process
  for(s in 1:nSites){
    for(t in 1:nYears){
      for(a in 1:3){ #stages
        y[s,t,a,1]~dbin(p[s,t,a],N[s,t,a]) #pass 1
        y[s,t,a,2]~dbin(p[s,t,a],N[s,t,a]-y[s,t,a,1]) #pass 2
        y[s,t,a,3]~dbin(p[s,t,a],N[s,t,a]-y[s,t,a,1]-y[s,t,a,2]) #pass 3
      }
    }
  }
}",file="~/lee/model.txt")