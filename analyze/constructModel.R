cat("model {
    
    # Specify the priors for all parameters in the model

    for(a in 1:3){
    lambda[a] ~ dunif(0, 200)    # Initial abundance for stage a
    omega[a] ~ dunif(0, 1)     #surival
    }

    #per capita recruitment
    gamma ~ dunif(0, 10) 

    # Detection probabilities for juveniles and adults
    p[1] ~ dunif(0, 1)
    p[2] ~ dunif(0, 1)
    
    # Create a loop across all j sites
    for(s in 1:nSites) {
      
      # Initiate the model for year 1 - Poisson with parameter
      # lambda
      # The abundance matrix N[s,t,a] is specified at location s in year t by stage a
      for(a in 1:3){  
        N[s,1,a] ~ dpois(lambda[a])
      
      # Create S and G vectors for year 1, which are not used in
      # the model. S and G for year one are set to be consistent
      # with the initial values and the presentation in the paper
        S[s,1,a] ~ dpois(20)
      }

        G[s,1] ~ dpois(2)
      
      # Specify the model for years 2 through nYears
      for(t in 2:nYears) {
        
        # Estimate survivorship
        S[s,t,1] ~ dbin(omega[1], N[s,t-1,1])
        S[s,t,2] ~ dbin(omega[1], N[s,t-1,2])
        S[s,t,3] ~ dbin(omega[2], N[s,t-1,3])
        
        # Estimate recruitment (gamma1)
        G[s,t] ~ dpois(gamma[1]*(N[s,t-1,3]+N[s,t-1,2]))

        
        # Sum all stages to get total N at location j in year t 
        N[s,t,1] <- G[s,t]
        N[s,t,2] <- S[s,t,1]
        N[s,t,3] <- S[s,t,2] + S[s,t,3]
      }
      
      # Loop across sampling replicates to estimate detection
      # probability for all years
      # The data matrix n is specified by observed class,
      # location, sampling replicate, year
      # by replicate
      for (t in 1:nYears){
          # detection probability is the same for all adults
          n[s,t,1,1] ~ dbin(p[1], N[s,t,1])
          n[s,t,1,2] ~ dbin(p[1], N[s,t,1]-n[s,t,1,1])
          n[s,t,1,3] ~ dbin(p[1], N[s,t,1]-n[s,t,1,1]-n[s,t,1,2])
        
          n[s,t,2,1] ~ dbin(p[2], N[s,t,2])
          n[s,t,2,2] ~ dbin(p[2], N[s,t,2]-n[s,t,2,1])
          n[s,t,2,3] ~ dbin(p[2], N[s,t,2]-n[s,t,2,1]-n[s,t,1,2])
          
          n[s,t,3,1] ~ dbin(p[2], N[s,t,3])
          n[s,t,3,2] ~ dbin(p[2], N[s,t,3]-n[s,t,3,1])
          n[s,t,3,3] ~ dbin(p[2], N[s,t,3]-n[s,t,3,1]-n[s,t,3,2])
        } }
    
    # Sum up the number of individuals in all locations to
    # estimate annual total N for each stage
    for (t in 1:nYears){
      Ntot[t,1] <- sum(N[,t,1])
      Ntot[t,2] <- sum(N[,t,2])
      Ntot[t,3] <- sum(N[,t,3])
    }
  }",file="~/lee/model.txt")