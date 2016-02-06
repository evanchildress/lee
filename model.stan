data{
  int<lower=1> n;
    int<lower=1> nTotalAges;
    int<lower=1> nYears;
    real<lower=0> length[n];
    real<lower=0> alpha[nYears,nTotalAges];
    }
    
    parameters{
    real<lower=0,upper=17> sigma[nTotalAges];
    simplex[nTotalAges] pu[nYears];
    real<lower=40> theta[nTotalAges-1];
    }
    
    transformed parameters{
    real mu[nTotalAges];
    mu[1]<-theta[1];
    for(g in 2:nTotalAges){
    mu[g]<-mu[g-1]+theta[g-1];
    }
    
    real ps[nTotalAges];
    }
    
    model{
    
    
    //priors
    for(g in 1:nTotalAges){
    for(t in 1:nYears){
    pu[g,t]~dirichlet(alpha[t]);
    }
    
    //likelihood
    for(i in 1:n){
    for(g in 1:nTotalAges){
    ps[g,year[i]]<-log(pu[g,year[i]])+normal_log(length[i],mu[g,year[i]],sigma[g,year[i]]);
    }
    increment_log_prob(log_sum_exp(ps));
    }
    }