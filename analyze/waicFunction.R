#returns widely applicable information criterion (waic, Watanabe 2010)
#for nMixture models or linear models (focus is on lower part of hierarchy)
#implemented based on Gelman's breakdown:
#http://www.stat.columbia.edu/~gelman/research/published/waic_understand3.pdf
waic<-function(out,y,paramNames=NULL,type="nMixture",grouped=F){
  sims<-out$BUGSoutput$sims.list
  y<-as.array(y)
  
  if(type=="nMixture"){
      p="p"
      lambda="lambda"
      N="N"
    if(!is.null(paramNames)){
      for(n in names(paramNames)){
        assign(n,paramNames[[n]])
      }
    }
    
    p<-sims[[p]]
    lambda<-sims[[lambda]]
    N<-sims[[N]]
    y<-array(rep(y,each=dim(N)[1]),dim=c(dim(N),3))
    probs<-array(NA,dim=dim(y))
  if(!grouped){
    probs[,,,,1]<-dbinom(y[,,,,1],N,p)*dpois(N,lambda)
    probs[,,,,2]<-dbinom(y[,,,,2],N-y[,,,,1],p)*dpois(N,lambda)
    probs[,,,,3]<-dbinom(y[,,,,3],N-y[,,,,1]-y[,,,,2],p)*dpois(N,lambda)
  } else {
    probs[,,,1]<-dbinom(y[,,,1],N,p)*dpois(N,lambda)
    probs[,,,2]<-dbinom(y[,,,2],N-y[,,,1],p)*dpois(N,lambda)
    probs[,,,3]<-dbinom(y[,,,3],N-y[,,,1]-y[,,,2],p)*dpois(N,lambda)
  }
    
  }
  
  
  if(type=="linearModel"){
    mu="mu"
    sigma="sigma"
  
    if(!is.null(paramNames)){
      for(n in names(paramNames)){
        assign(n,paramNames[[n]])
      }
    }
    mu<-sims[[mu]]
    sigma<-sims[[sigma]]
    y<-array(rep(y,each=dim(mu)[1]),dim=dim(mu))
    
    probs<-array(NA,dim=dim(y))
    probs<-dnorm(y,mu,sigma)
  }
  
  dimsToApply<-2:length(dim(probs))
  pd<-sum(apply(log(probs),dimsToApply,var),na.rm=T)
  waic<- -2*(sum(log(1/dim(probs)[1]*apply(probs,dimsToApply,sum)),na.rm=T)-pd)
  return(waic)
}