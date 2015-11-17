phi<-c(0.3,0.7,0.7)
alpha<-1.1

makeData<-function(phi,nSites,p,lambda,alpha,nYears=20){
  
  #make empty array
  N<-array(NA,dim=c(nSites,nYears,3))
  y<-array(NA,dim=c(nSites,nYears,3,3))
  
  #initial abundance
    for(a in 1:3){
      N[,1,a]<-rpois(nSites,lambda[a])
    }
  
  #subsequent abundance

    for(t in 1:(nYears-1)){
        N[,t+1,1]<-rpois(nSites,(N[s,t,2]+N[s,t,3])*alpha)
        N[,t+1,2]<-rbinom(nSites,N[s,t,1],phi[1])
        N[,t+1,3]<-rbinom(nSites,N[s,t,2],phi[2])+rbinom(nSites,N[s,t,3],phi[3])
    }
  
  #detection
  for(s in 1:nSites){
    for(t in 1:nYears){
      for(a in 1:3){
        y[s,t,a,1]<-rbinom(1,N[s,t,a],p)
        y[s,t,a,2]<-rbinom(1,N[s,t,a]-y[s,t,a,1],p)
        y[s,t,a,3]<-rbinom(1,N[s,t,a]-y[s,t,a,1]-y[s,t,a,2],p)
      }
    }
  }
  
return(list(N=N,nSites=nSites,nYears=nYears,y=y,lambda=lambda,alpha=alpha,phi=phi,p=p))
}

data<-makeData(phi,nSites=30,p=0.9,lambda=c(40,30,30),alpha=alpha)

win.data<-list(n=data$y[,11:20,,],nYears=10,nSites=30)

totalN<-apply(data$N,c(1,2),sum)
plot(NA,xlim=c(1,20),ylim=c(0,300))
for(s in 1:nrow(totalN)){
  for(a in 1:3){
    points(data$N[s,,a],type='l',col=palette()[a])
  }
}
