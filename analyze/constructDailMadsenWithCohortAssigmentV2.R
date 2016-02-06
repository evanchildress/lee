cat("
model{
#Cohort assignment
    for(t in 1:nYears){ #fixed year effect
      for(g in 1:nTotalAges){
        theta[g,t]~dnorm(0,0.001)I(0,) #difference between mu[g-1,] and mu[g,] (i.e. mu[1,]<mu[2,]<mu[3,]...)
      }
      muSize[1,t]<-theta[1,t]+40 #sets a lower limit of 40 on mu[1,]
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
  for(e in 1:nEval){
    for(i in 1:eval[e,4]){ #total fish caught in each site, year, pass
      age[i,eval[e,1],eval[e,2],eval[e,3]]~dcat(pu[eval[e,2],])
      length[i,eval[e,1],eval[e,2],eval[e,3]]~
            dnorm(muSizeSite[age[i,eval[e,1],eval[e,2],eval[e,3]],eval[e,2],eval[e,1]],
                  tau[age[i,eval[e,1],eval[e,2],eval[e,3]]])
    }
  }

  
# #Create counts for dail madsen model by site, year, stage,and pass
  for(e in 1:nEval){
    for(i in 1:eval[e,4]){ #total fish caught in each site, year, pass
      for(a in 1:(nAges-1)){
        thisAge[i,eval[e,1],eval[e,2],a,eval[e,3]]<-
                equals(age[i,eval[e,1],eval[e,2],eval[e,3]],a)
      }
      thisAge[i,eval[e,1],eval[e,2],nAges,eval[e,3]]<-
              step(age[i,eval[e,1],eval[e,2],eval[e,3]]-nAges) #lump older cohorts
    }
        for(a in 1:nAges){
            counts[eval[e,1],eval[e,2],a,eval[e,3]]<-sum(thisAge[1:eval[e,4],
                                                                 eval[e,1],
                                                                 eval[e,2],
                                                                 a,
                                                                 eval[e,3]])
            y[eval[e,1],eval[e,2],a,eval[e,3]]~
                  dbin(counts[eval[e,1],eval[e,2],a,eval[e,3]],1)
#             zeros[eval[e,1],eval[e,2],a,eval[e,3]]<-0
#             zeros[eval[e,1],eval[e,2],a,eval[e,3]]~
#                   dpois(lambda[eval[e,1],eval[e,2],a,eval[e,3]])
#             lambda[eval[e,1],eval[e,2],a,eval[e,3]]<-
#                   y[eval[e,1],eval[e,2],a,eval[e,3]]
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
#     for(a in 1:nAges){
#       phiSigma[a]~dunif(0,10)#variance for random site effect on surival
#       phiTau[a]<-1/pow(phiSigma[a],2)
#     }
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
    
    #1+ in t+1 split into maturing yoy, surviving 1+, (and could add arriving 1+)
        S[s,t,1]~dbin(phi[s,t,1],N[s,t,1]) #apparent survival of yoy
        S[s,t,2]~dbin(phi[s,t,2],N[s,t,2]) #apparent survival of 1+ year olds
        N[s,t+1,2]<-S[s,t,1]+S[s,t,2] #summed adults from all processes
      }
    }
    
    #Observation process
    for(s in 1:nSites){
      for(t in 1:nYears){
        for(a in 1:nAges){ #stages
          y[s,t,a,1]~dbin(cp[s,t,a],N[s,t,a]) #pass 1

          N2[s,t,a]<-N[s,t,a]-y[s,t,a,1] #fish left after pass 1
          y[s,t,a,2]~dbin(cp[s,t,a],N2[s,t,a]) #pass 2

          N3[s,t,a]<-N[s,t,a]-y[s,t,a,1]-y[s,t,a,2] #fish left after pass 1+2
          y[s,t,a,3]~dbin(cp[s,t,a],N3[s,t,a]) #pass 3
        }
      }
    }
  
    }",file='~/lee/analyze/model.bugs')
