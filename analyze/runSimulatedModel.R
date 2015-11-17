require(R2jags)

siteTotals<-apply(win.data$n[,1,,],c(1,2),sum)
maxStart<-apply(siteTotals,2,max)+10


inits<-function(){
  list(lambda=maxStart,
       gamma=1.1,
       omega=c(0.3,0.7,0.7),
       p=c(0.9,0.9)
  )
}

params<-c("N","lambda","gamma","omega","p")

ni=2000
nt=3
nb=500
nc=3

out<-jags(win.data,inits=inits,params,"~/lee/model.txt",n.chains=nc,n.iter=ni,
          n.thin=nt,n.burnin=nb,working.directory=getwd())
