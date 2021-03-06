cat("
model{
#Cohort assignment
    for(t in 1:nYears){ #fixed year effect
      for(g in 1:nTotalAges){
        theta[g,t]~dnorm(0,0.001)I(0,) #difference between mu[g-1] and mu[g] (i.e. mu[1]<mu[2]<mu[3]...)
      }
      muSize[1,t]<-theta[1,t]+40
    for(g in 2:nTotalAges){
    #overall mean length for each cohort in each year
        muSize[g,t]<-muSize[g-1,t]+theta[g,t]+45

      }#g
    #fixed year effect on pu (inclusion in group g)
          pu[t,1:nTotalAges]~ddirch(alph[])
      
    }#t

    for(g in 1:nTotalAges){
    #variance
        sigma[g]~dunif(0,16)
        tau[g]<-1/pow(sigma[g],2)
    }


    
    for(t in 1:nYears){
      for(g in 1:nTotalAges){
        for(s in 1:nSites){
          muSizeSite[g,t,s]<-muSize[g,t]
        }
      }
    }
    
    #likelihood
    for(i in 1:n){
      age[i]~dcat(pu[year[i],])
      length[i]~dnorm(muSizeSite[age[i],year[i],site[i]],tau[age[i]])
    }
  
# #Create counts for dail madsen model by site, year, stage,and pass
# #This is a really sluggish way to slot the data into a count array
  for(i in 1:n){
    for(s in 1:nSites){
      thisSite[s,i]<-equals(site[i],s)
    }
    for(t in 1:nYears){
      thisYear[t,i]<-equals(year[i],t)
    }

    for(a in 1:(nAges-1)){
      thisAge[a,i]<-equals(age[i],a)
    }
    thisAge[nAges,i]<- step(age[i]-nAges) #lump the older ages

    for(p in 1:nPasses){
      thisPass[p,i]<-equals(pass[i],p)
    }
  } 
  
  for(s in 1:nSites){
    for(t in 1:nYears){
      for(p in 1:nPasses){
          for(i in 1:totalY[s,t,p]){
            thisAge[s,t,p,i,a]<-equals(thisSite[s,i]+thisYear[t,i]+thisAge[a,i]+thisPass[p,i],4)
          }
            counts[s,t,a,p]<-sum(thisEverything[s,t,a,p,])
            y[s,t,a,p]<-cut(counts[s,t,a,p])
        }
      }
    }
  }


#stage structured Dail Madsen model using counts (derived from modeled ages)
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
      for(a in 1:1){
        alphaMu[t,a]~dnorm(0,0.001) #fixed effect for year
    
        for(s in 1:nSites){
          logAlpha[s,t,a]~dnorm(alphaMu[t,a],alphaTau) #random site effect
          alpha[s,t,a]<-exp(logAlpha[s,t,a])
        }#s
      }#a
    }#t
    
    #detection, depends only on siteWidth with random site effect
    for(b in 1:3){ #2 betas intercept and slope for siteWidth
      for(a in 1:nAges){
        betaP[b,a]~dnorm(0,0.37) #Jeffrey's prior, uninformative on logit scale
      }
    }
    
    for(s in 1:nSites){
      for(t in 1:nYears){
        for(a in 1:nAges){
          logitP[s,t,a]<-betaP[1,a]+betaP[2,a]*siteWidth[s,t]
          cp[s,t,a]<-1/(1+exp(logitP[s,t,a]))
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
          counts[s,t,a,1]~dbin(cp[s,t,a],N[s,t,a]) #pass 1

          N2[s,t,a]<-N[s,t,a]-counts[s,t,a,1]
          counts[s,t,a,2]~dbin(cp[s,t,a],N2[s,t,a]) #pass 2

          N3[s,t,a]<-N[s,t,a]-counts[s,t,a,1]-counts[s,t,a,2]
          counts[s,t,a,3]~dbin(cp[s,t,a],N3[s,t,a]) #pass 3
        }
      }
    }
  
    }",file='~/lee/analyze/model.bugs')
