hier_mnl = function (Data, Prior, Mcmc, Cont) {
  # This function implements a random-walk Metropolis-Hastings algorithm for a hierarchical
  # MNL with a multivariate normal distribution of heterogeneity.
  
  # Describe and Assign Function Arguments ----------------------------------
  # Data = list(data,Z).
  data = Data$data                                        # List of lists with choices and design matrices.
  Z = Data$Z                                              # Covariates for the upper-level model.
  if (Mcmc$sim_ind==1) {
    Beta = Data$Beta                                      # True values of Beta for parameter recovery.
    Gamma = Data$Gamma                                    # True values of Gamma for parameter recovery.
    Vbeta = Data$Vbeta                                    # True values of Vbeta for parameter recovery.
  }
  
  # Prior = list(gammabar,Agamma,nu,V).
  gammabar = Prior$gammabar                               # Means for normal prior on Gamma.
  Agamma = Prior$Agamma                                   # Precision matrix for normal prior on Gamma.
  nu = Prior$nu                                           # DF for IW prior on Vbeta.
  V = Prior$V                                             # Location for IW prior on Vbeta.
  
  # Mcmc = list(R,keep,step,sim_ind,cont_ind).
  R = Mcmc$R                                              # Number of iterations in the Markov chain.
  keep = Mcmc$keep                                        # Thinning parameter.
  step = Mcmc$step                                        # RW step (scaling factor) for the beta draws.
  sim_ind = Mcmc$sim_ind                                  # Indicates a simulation experiment.
  cont_ind = Mcmc$cont_ind                                # Indicates a run continuation.
  
  # Assign values from the function arguments.
  nresp = length(data)                                    # Number of respondents.
  nscns = length(data[[1]]$y)                             # Number of choice tasks.
  nalts = length(data[[1]]$X[,1])/nscns                   # Number of alternatives in each choice task.
  nvars = ncol(data[[1]]$X)                               # Number of attribute levels.
  ncovs = ncol(Z)                                         # Number of covariates for the upper level.
  
  # Describe and Initialize Function Output ---------------------------------
  # Respondent-level parameter draws.
  betadraw = array(double(floor(R/keep)*nresp*nvars),dim=c(nresp,nvars,floor(R/keep)))
  
  # Aggregate-level parameter draws.
  Gammadraw = matrix(double(floor(R/keep)*nvars*ncovs),ncol=nvars*ncovs)
  Vbetadraw = matrix(double(floor(R/keep)*nvars*nvars),ncol=nvars*nvars)
  
  # Diagnostic draws and initial clock time.
  llikedraw = double(floor(R/keep))     # Log likelihood.
  baccept = array(0,dim=c(R/keep))      # Beta acceptance rate.
  bstepdraw = array(0,dim=c(R/keep))    # RW step adjusted during burn-in.
  itime = proc.time()[3]                # Initial clock time.
  
  # Initialize MCMC ---------------------------------------------------------
  cat("MCMC Iteration (estimated time to end in min | step | baccept | ll )",fill=TRUE)
  
  # Initialize values.
  if (cont_ind == 0) {
    if (sim_ind==0) oldbetas = matrix(double(nresp*nvars),ncol=nvars)
    if (sim_ind==1) oldbetas = matrix(Beta,ncol=nvars)
    oldGamma = matrix(double(nvars*ncovs),ncol=nvars)
    oldVbeta = diag(nvars)
    oldVbetai = diag(nvars)
  }
  
  # Initialize values and use the previous draws for continued runs.
  if (cont_ind == 1) {
    step = Cont$out_step
    oldbetas = Cont$out_oldbetas
    oldGamma = matrix(Cont$out_oldgamma,ncol=nvars)
    oldVbeta = Cont$out_oldVbeta
    oldVbetai = Cont$out_oldVbeta
  }
  
  # Run the MCMC ------------------------------------------------------------
  # The Markov chain will run for R iterations.
  for (rep in 1:R) {
    # Initial log likelihood values for each iteration.
    logold = lognew = 0
    loglike = 0
    
    # Respondent-level loop.
    bnaccept = 0
    for (resp in 1:nresp) {
      # Beta old and candidate draws.
      betad = oldbetas[resp,]
      betac = betad + mvrnorm(1,rep(0,nvars),(step)*oldVbeta)
      
      # Log likelihood with the old gamma draws and old/candidate beta draws.
      logold = llmnl(betad,data[[resp]]$y,data[[resp]]$X)
      lognew = llmnl(betac,data[[resp]]$y,data[[resp]]$X)
      
      # Log of the MVN distribution of heterogeneity over all betas.
      loghold = -0.5*(t(betad)-Z[resp,]%*%oldGamma)%*%oldVbetai%*%(betad-t(Z[resp,]%*%oldGamma))
      loghnew = -0.5*(t(betac)-Z[resp,]%*%oldGamma)%*%oldVbetai%*%(betac-t(Z[resp,]%*%oldGamma))
      
      # Beta log posteriors.
      lpostold = logold + loghold
      lpostnew = lognew + loghnew
      
      # Compare the old and candidate posteriors and compute alpha (second-stage prior cancels out).
      diff = exp((lpostnew) - (lpostold))
      if (diff == "NaN" || diff == Inf) {
        alpha = -1 # If the number doesn't exist, always reject.
      } else {
        alpha = min(1,diff)
      }
      unif = runif(1)
      if (unif < alpha) {
        oldbetas[resp,] = betac
        bnaccept = bnaccept + 1
        loglike = loglike + lognew
      } else {
        loglike = loglike + logold
      }
    }
    
    # Gamma and Vbeta draw (distribution of heterogeneity over beta).
    out = rmultireg(oldbetas,Z,gammabar,Agamma,nu,V)
    oldGamma = out$B
    oldVbeta = out$Sigma
    oldVbetai = chol2inv(chol(oldVbeta))
    
    # Houskeeping and Output --------------------------------------------------
    # Modify the RW step sizes to constrain acceptance rates during burn-in (R/3).
    if (rep%%5 == 0 & cont_ind == 0) {
      if (rep < (R/2)) {
        # Update step.
        if (bnaccept/nresp < .20) {
          step = step*0.95
        }
        if (bnaccept/nresp > .60) {
          step = step*1.05
        }
      }
    }
    
    # Print progress.
    if (rep%%5 == 0) {
      ctime = proc.time()[3]
      timetoend = ((ctime - itime)/rep)*(R - rep)
      bacceptr=bnaccept/nresp
      cat(" ",rep," (",round(timetoend/60,2),"|",round(step,5),"|",round(bacceptr,2),"|",round(loglike,2),")",fill = TRUE)
    }
    
    # Print chart less often.
    if (rep%%100 == 0) {
      par(mfrow=c(2,1))
      if (sim_ind==0) { plot(llikedraw,type="l"); matplot(Gammadraw,type="l") }
      if (sim_ind==1) {
        plot(llikedraw,type="l")
        matplot(Gammadraw[,1:ncovs],type="l",col=c(1:ncovs)); abline(h=Data$Gamma[,1],col=c(1:ncovs))
      }
    }
    
    # Save the posterior draws.
    mkeep = rep/keep
    if (mkeep*keep == (floor(mkeep)*keep)) {
      betadraw[,,mkeep] = oldbetas
      Gammadraw[mkeep,] = as.vector(oldGamma)
      Vbetadraw[mkeep,] = as.vector(oldVbeta)
      llikedraw[mkeep] = loglike
      baccept[mkeep] = bnaccept/nresp
      bstepdraw[mkeep] = step
    }
    
    # Save out continuation files.
    if (rep%%R == 0) {
      Cont = list(out_oldbetas = betadraw[,,R/keep],out_oldgamma = matrix(Gammadraw[R/keep,],byrow=TRUE,ncol=(nvars*ncovs)),
           out_oldVbeta = matrix(Vbetadraw[R/keep,],byrow=TRUE,ncol=nvars),out_step = step)
    }
  }
  
  # Print total run time.
  ctime = proc.time()[3]
  cat(" Total Time Elapsed (in Minutes): ",round((ctime - itime)/60,2),fill = TRUE)
  
  # Output.
  return(list(betadraw=betadraw,Gammadraw=Gammadraw,Vbetadraw=Vbetadraw,
    llikedraw=llikedraw,baccept=baccept,bstepdraw=bstepdraw,Cont=Cont))
}