makeModel<-function(type="fixedYear"){
if(type=="fixedYear"){
  cat("model{
    #Priors
    #detection (different for yoy and adults but constant across time and space)
    for(a in 1:nAges){
      for(b in 1:2){
        betaP[b,a]~dnorm(0,0.01)
      }
    #overallMean
    #mu[a]~dnorm(0,0.01)
    #random site effect
      for(i in 1:nSites){
        muSite[i,a]~dnorm(0,tauSite[a])
      }

      sdSite[a]~dunif(0,15)
      tauSite[a]<-1/pow(sdSite[a],2)
    
    #fixed year effect
      for(j in 1:nYears){
        muYear[j,a]~dnorm(0,0.01)
      }

    #slopes for covariates on lambda
#       for(b in 1:2){
#         for(j in 1:nYears){
#           betaLambda[b,j,a]~dnorm(0,0.01)
#           #betaLam[b,a]<-0
# 
#           #betaLambda[b,j,a]<-betaLam[b,a]
#         }
#       }
    }
    
    
    #Likelihood
    
    for(i in 1:nSites){
      for(j in 1:nYears){
        for(a in 1:nAges){

    #abundance
          N[i,j,a]~dpois(lambda[i,j,a])
          log(lambda[i,j,a])<-muSite[i,a]+muYear[j,a]+
#                               betaLambda[1,j,a]*covariates[i,1]+
#                               betaLambda[2,j,a]*covariates[i,2]+
                              log(meanSiteWidth[i])
    
    #Observation for depletion sampling
    
          y[i,j,a,1]~dbin(p[i,j,a],N[i,j,a])
          y[i,j,a,2]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1])
          y[i,j,a,3]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1]-y[i,j,a,2])
          logitP[i,j,a]<-betaP[1,a]+siteWidth[i,j]*betaP[2,a]
          p[i,j,a]<-1/(1+exp(-logitP[i,j,a]))
        }
      }
    }
  for(i in 1:nSites){
    for(j in 1:nYears){
      for(a in 1:nAges){
        nNew[i,j,a]~dpois(lambda[i,j,a])
        yNew[i,j,a,1]~dbin(p[i,j,a],N[i,j,a])
        yNew[i,j,a,2]~dbin(p[i,j,a],N[i,j,a]-yNew[i,j,a,1])
        yNew[i,j,a,3]~dbin(p[i,j,a],N[i,j,a]-yNew[i,j,a,1]-yNew[i,j,a,2])
      }
    }
  }
    }",
  file="model.txt")}

  if(type=="mean"){
  cat("model{
    #Priors
      #detection (different for yoy and adults but constant across time and space)
      for(a in 1:nAges){
        for(b in 1:2){
          betaP[b,a]~dnorm(0,0.01)
        }
      #overallMean
        mu[a]~dnorm(0,0.01)
      #random site effect
        for(i in 1:nSites){
          muSite[i,a]~dnorm(0,tauSite[a])
        }
      
      sdSite[a]~dunif(0,15)
      tauSite[a]<-1/pow(sdSite[a],2)

      #slopes for covariates on lambda
#       for(b in 1:2){
#         betaLambda[b,a]~dnorm(0,0.01)
#         }
      }
      
      
      #Likelihood
      
      for(i in 1:nSites){
        for(j in 1:nYears){
          for(a in 1:nAges){
      
      #abundance
            N[i,j,a]~dpois(lambda[i,j,a])
            log(lambda[i,j,a])<-mu[a]+muSite[i,a]+
#                                 betaLambda[1,a]*covariates[i,1]+
#                                 betaLambda[2,a]*covariates[i,2]+
                                meanSiteWidth[i]
      
      #Observation for depletion sampling
      
            y[i,j,a,1]~dbin(p[i,j,a],N[i,j,a])
            y[i,j,a,2]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1])
            y[i,j,a,3]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1]-y[i,j,a,2])
            logitP[i,j,a]<-betaP[1,a]+siteWidth[i,j]*betaP[2,a]
            p[i,j,a]<-1/(1+exp(-logitP[i,j,a]))
          }
        }
      }
}",
file="model.txt")}
if(type=="linearYear"){  
  cat("model{
    #Priors
      #detection (different for yoy and adults but constant across time and space)
      for(a in 1:nAges){
        for(b in 1:2){
          betaP[b,a]~dnorm(0,0.01)
        }
      #overallIntercept
        mu[a]~dnorm(0,0.01)
      #random site effect
        for(i in 1:nSites){
          muSite[i,a]~dnorm(0,tauSite[a])
        }
      
        sdSite[a]~dunif(0,15)
        tauSite[a]<-1/pow(sdSite[a],2)
      
      #linear year effect
        betaYear[a]~dnorm(0,0.01)
      
      #slopes for covariates on lambda
#         for(b in 1:2){
#           betaLambda[b,a]~dnorm(0,0.01)
#         }
      }
      
      
      #Likelihood
      
      for(i in 1:nSites){
        for(j in 1:nYears){
          for(a in 1:nAges){
      
      #abundance
            N[i,j,a]~dpois(lambda[i,j,a])
            log(lambda[i,j,a])<-mu[a]+muSite[i,a]+betaYear[a]*j+
#                                 betaLambda[1,a]*covariates[i,1]+
#                                 betaLambda[2,a]*covariates[i,2]+
                                meanSiteWidth[i]
                                
      #Observation for depletion sampling
      
            y[i,j,a,1]~dbin(p[i,j,a],N[i,j,a])
            y[i,j,a,2]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1])
            y[i,j,a,3]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1]-y[i,j,a,2])
            logitP[i,j,a]<-betaP[1,a]+siteWidth[i,j]*betaP[2,a]
            p[i,j,a]<-1/(1+exp(-logitP[i,j,a]))
          }
        }
      }
}",
file="model.txt")}

if(type=="linearYearRandomSlope"){  
  cat("model{
      #Priors
      #detection (different for yoy and adults but constant across time and space)
      for(a in 1:nAges){
      for(b in 1:2){
      betaP[b,a]~dnorm(0,0.01)
      }
      #overallIntercept
      mu[a]~dnorm(0,0.01)
      #random site effect
      for(i in 1:nSites){
      muSite[i,a]~dnorm(0,tauSite[1,a])
      betaYear[i,a]~dnorm(betaYearMean[a],tauSite[2,a])
      }
      for(b in 1:2){
      sdSite[b,a]~dunif(0,15)
      tauSite[b,a]<-1/pow(sdSite[b,a],2)
      }

      #linear year effect
      betaYearMean[a]~dnorm(0,0.01)
      
      #slopes for covariates on lambda
#       for(b in 1:2){
#       betaLambda[b,a]~dnorm(0,0.01)
#       }
      }
      
      
      #Likelihood
      
      for(i in 1:nSites){
      for(j in 1:nYears){
      for(a in 1:nAges){
      
      #abundance
      N[i,j,a]~dpois(lambda[i,j,a])
      log(lambda[i,j,a])<-mu[a]+muSite[i,a]+betaYear[i,a]*j+
#       betaLambda[1,a]*covariates[i,1]+
#       betaLambda[2,a]*covariates[i,2]+
      meanSiteWidth[i]
      
      #Observation for depletion sampling
      
      y[i,j,a,1]~dbin(p[i,j,a],N[i,j,a])
      y[i,j,a,2]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1])
      y[i,j,a,3]~dbin(p[i,j,a],N[i,j,a]-y[i,j,a,1]-y[i,j,a,2])
      logitP[i,j,a]<-betaP[1,a]+siteWidth[i,j]*betaP[2,a]
      p[i,j,a]<-1/(1+exp(-logitP[i,j,a]))
      }
      }
      }
}",
file="model.txt")}
  
    
}