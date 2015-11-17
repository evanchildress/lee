cat("model{
    #Priors
    #detection (different for yoy and adults but constant across time and space)
    for(k in 1:2){
      for(n in 1:2){
        beta[n,k]~dnorm(0,0.1)
      }
    
    #overall mean abundance on log scale
      mu[k]~dunif(-10,10)
    #random site effect
      for(i in 1:nSites){
        muSite[i,k]~dnorm(0,tauSite[k])
      }

      sdSite[k]~dunif(0,15)
      tauSite[k]<-1/pow(sdSite[k],2)
    
    #random year effect
      for(j in 1:nYears){
        muYear[j,k]~dnorm(0,tauYear[k])
      }

      sdYear[k]~dunif(0,15)
      tauYear[k]<-1/pow(sdYear[k],2)
    }
    
    
    #Likelihood
    
    for(i in 1:nSites){
      for(j in 1:nYears){
        for(k in 1:2){

    #abundance
          N[i,j,k]~dpois(lambda[i,j,k])
          log(lambda[i,j,k])<-mu[k]+muSite[i,k]+muYear[j,k]
    
    #Observation for depletion sampling
    
          y[i,j,k,1]~dbin(p[i,j,k],N[i,j,k])
          y[i,j,k,2]~dbin(p[i,j,k],N[i,j,k]-y[i,j,k,1])
          y[i,j,k,3]~dbin(p[i,j,k],N[i,j,k]-y[i,j,k,1]-y[i,j,k,2])
          logitP[i,j,k]<-beta[1,k]+siteWidth[i,j]*beta[2,k]
          p[i,j,k]<-1/(1+exp(-logitP[i,j,k]))
        }
      }
    }
    }",
file="model.txt")