cat( #Dale-Madsen model for lee with 2 stages
"model{
  #data inputs:
#   nYears
#   nSites
#   nAges
#   y dim=c(nSites,nYears,nAges,nPasses)
#   siteWidth dim=c(nSites,nYears) standardized to have mean zero and sd 1
  
  
  #Priors
  
  #initial abundance parameters
  for(a in 1:nAges){
    lambdaMu[a]~dnorm(0,0.01) #average abundance for starting year
    lambdaSigma[a]~dunif(0,5) #variance for random site effect for starting abundance
    lambdaTau[a]<-1/pow(lambdaSigma[a],2)
    
    for(s in 1:nSites){
      logLambda[s,a]~dnorm(lambdaMu[a],lambdaTau[a]) #random site effect for initial abundance
      lambda[s,a]<-exp(logLambda[s,a])
    }
  }
  
  #survival parameters
  for(a in 1:nAges){
    phiSigma[a]~dunif(0,10)#variance for random site effect on surival
    phiTau[a]<-1/pow(phiSigma[a],2)
  }
  for(t in 1:(nYears-1)){
    for(a in 1:nAges){ 
      phiMu[t,a]~dnorm(0,0.37) #average phi for each time/stage

      
      for(s in 1:nSites){
        #logitPhi[s,t,a]~dnorm(phiMu[t,a],phiTau[a]) #random site effect on survival constant across time 
        logitPhi[s,t,a]<-phiMu[t,a]
        phi[s,t,a]<-1/(1+exp(logitPhi[s,t,a]))      
      }
    }
  }
  
  #arrivals, a=1 is reproduction (in place or somewhere else), could have a=2 for adult immigration

  alphaSigma~dunif(0,10) #variation on random site effect
  alphaTau<-1/pow(alphaSigma,2)

  for(t in 1:(nYears-1)){
    for(a in 1){
      alphaMu[t,a]~dnorm(0,0.001) #fixed effect for year

      for(s in 1:nSites){
        
        logAlpha[s,t,a]~dnorm(alphaMu[t,a],alphaTau) #random site effect
        alpha[s,t,a]<-exp(logAlpha[s,t,a])
      }
    }
  }
  
  #detection, depends only on siteWidth with random site effect
  for(b in 1:2){ #2 betas intercept and slope for siteWidth
    for(a in 1:nAges){
      beta[b,a]~dnorm(0,0.37) #Jeffrey's prior, uninformative on logit scale
    }
  }

  for(s in 1:nSites){
    for(t in 1:nYears){
      for(a in 1:nAges){
        logitP[s,t,a]<-beta[1,a]+beta[2,a]*siteWidthOverall[s,t]
        p[s,t,a]<-1/(1+exp(logitP[s,t,a]))
      }
    }
  }
  
  #Likelihood
  
  #State process
  #establish initial abundance for each stage
  for(s in 1:nSites){
    for(a in 1:nAges){
      N[s,1,a]~dpois(lambda[s,a])
    }
  }
  
  #loop through subsequent years
  for(s in 1:nSites){
    for(t in 1:(nYears-1)){
      #stages: 1=yoy, 2=1+
      #yoy abundance is only reproduction (or arrival of reproduced inviduals)
      N[s,t+1,1]~dpois(alpha[s,t,1])
      
      #1+ in t+1 split into maturing yoy, surviving 1+, and arriving 1+
      S[s,t,1]~dbin(phi[s,t,1],N[s,t,1]) #apparent survival of yoy
      S[s,t,2]~dbin(phi[s,t,2],N[s,t,2]) #apparent survival of 1+ year olds
      N[s,t+1,2]<-S[s,t,1]+S[s,t,2] #summed adults from all processes
    }
  }
  
  #Observation process
  for(s in 1:nSites){
    for(t in 1:nYears){
      for(a in 1:nAges){ #stages
        y[s,t,a,1]~dbin(p[s,t,a],N[s,t,a]) #pass 1
        y[s,t,a,2]~dbin(p[s,t,a],N[s,t,a]-y[s,t,a,1]) #pass 2
        y[s,t,a,3]~dbin(p[s,t,a],N[s,t,a]-y[s,t,a,1]-y[s,t,a,2]) #pass 3
      }
    }
  }
}",file="~/lee/model.txt")