# This code implements a hierarchical MNL choice model with a MVN distribution of heterogeneity.

# Preamble ----------------------------------------------------------------
library(bayesm); library(MASS)
rm(list=ls()); set.seed(31)

# Import Robotic Vacuum data.
load(file="Data/Robotic Vacuums/choice_data.RData"); load(file="Data/Robotic Vacuums/covariates.RData")


# Simulation --------------------------------------------------------------
dim_ind = 1 # Indicates the simulation should match the actual dimensions of the data.

# Not using the dimensions of the data.
if (dim_ind==0) {
  nresp = 200                                  # Number of respondents.
  nscns = 30                                   # Number of choice tasks.
  nalts = 4                                    # Number of alternatives in each choice task.
  nvars = 8                                    # Number of attribute levels.
  ncovs = 3                                    # Number of covariates for the upper level.
}

# Using the dimensions of the data.
if (dim_ind==1) {
  nresp = length(choice_data)                  # Number of respondents.
  nscns = length(choice_data[[1]]$y)           # Number of choice tasks.
  nalts = length(choice_data[[1]]$X[,1])/nscns # Number of alternatives in each choice task.
  nvars = ncol(choice_data[[1]]$X)             # Number of attribute levels.
  ncovs = ncol(covariates$attitudes)+1         # Number of covariates for the upper level.
}

# Mean-centered covariates with an intercept for the distribution of heterogeneity.
if (dim_ind==0) { 
  Z = matrix(round(runif(nresp*(ncovs-1))),nrow=nresp,ncol=(ncovs-1))
  Z_mean = apply(Z,2,mean)
  for (i in 1:ncol(Z)) {
    Z[,i] = Z[,i] - Z_mean[i]
  }
  Z = cbind(rep(1,nrow(Z)),Z)
}
if (dim_ind==1) { 
  Z = covariates$attitudes
  Z_mean = apply(Z,2,mean)
  for (i in 1:ncol(Z)) {
    Z[,i] = Z[,i] - Z_mean[i]
  }
  Z = cbind(rep(1,nrow(Z)),Z)
}

# True Gamma (ncovs x nvars) and Vbeta (nvars x nvars) values.
Gamma = matrix(runif(ncovs*nvars,-4,3),byrow=FALSE,nrow=ncovs,ncol=nvars)
iota = matrix(1,nrow=nvars,ncol=1); Vbeta = diag(nvars) + .5*iota%*%t(iota)

# Generate data, each respondent with a list of y and X.
data = NULL; Beta = NULL
for (resp in 1:nresp) {
  y = NULL; X = NULL
  y_scn = matrix(nrow=nalts,ncol=1)
  
  # Generate individual-level betas.
  beta = t(Gamma)%*%Z[resp,] + as.vector(t(chol(Vbeta))%*%rnorm(nvars))
  
  # Compute the latent utility a scenario at a time.
  for (scn in 1:nscns) {
    if (dim_ind==0) X_scn = matrix(round(runif(nalts*nvars)),nrow=nalts,ncol=nvars)
    if (dim_ind==1) X_scn = choice_data[[resp]]$X[(scn*nalts-nalts+1):(scn*nalts),]
    
    # Compute and the latent utility for each alternative and find the max.
    U = X_scn%*%beta + matrix((-log(-log(runif(nalts)))),ncol=1)
    y_scn = which(U==max(U))
    
    # Bind the response vector and design matrix.
    y = rbind(y,y_scn)
    X = rbind(X,X_scn)
  }
  
  # Save out each respondent's list.
  data[[resp]] = list(y=y,X=X)
  Beta = rbind(Beta,matrix(beta,ncol=nvars))
}

# Recover parameters values.
Data = list(data=data,Z=Z,Beta=Beta,Gamma=Gamma,Vbeta=Vbeta)
Prior = list(gammabar=matrix(rep(0,ncovs*nvars),ncol=nvars),Agamma=0.01*diag(ncovs),nu=nvars+3,V=(nvars+3)*diag(nvars))
Mcmc = list(R=5000,keep=5,step=.08,sim_ind=1,cont_ind=0)

source("Estimation/1 Hierarchical MNL/hier_mnl.R"); outtest = hier_mnl(Data,Prior,Mcmc)

# Aggregate draws.
par(mfrow=c(3,1))
matplot(outtest$llikedraw,type="l",main="3k iterations hier_mnl",ylab="Log Likelihood")
matplot(outtest$Gammadraw,type="l",col=c(1:(ncovs*nvars)),ylab="Gamma Draws"); abline(h=Gamma,col=c(1:(ncovs*nvars)))
matplot(outtest$Vbetadraw[,1:nvars],type="l",col=c(1:nvars),ylab="Vbeta Draws"); abline(h=Vbeta[,1:nvars],col=c(1:nvars))

# Gamma credible intervals.
Gamma_Vec = matrix(Gamma,nrow=1,ncol=(nvars*ncovs))
cat("95% Credible Intervals for Gammadraws",fill=TRUE)
credint = matrix(nrow=(nvars*ncovs),ncol=3); R=Mcmc$R/Mcmc$keep
for (i in 1:(nvars*ncovs)) {
  credint[i,1] = quantile(outtest$Gammadraw[round(2*(R/3)):R,i],.025)
  credint[i,2] = quantile(outtest$Gammadraw[round(2*(R/3)):R,i],.975)
  if ((as.numeric(credint[i,1]) <= Gamma_Vec[i])&(Gamma_Vec[i] <= as.numeric(credint[i,2]))) credint[i,3] = 1
  if ((as.numeric(credint[i,1]) > Gamma_Vec[i])|(Gamma_Vec[i] > as.numeric(credint[i,2]))) credint[i,3] = 0
}
sumtable = cbind(matrix(cbind(Gamma_Vec,t(apply(outtest$Gammadraw[round(2*(R/3)):R,],2,mean))),ncol=2),credint)
colnames(sumtable) = c("True","Mean","Lower","Upper","In"); sumtable; sum(sumtable[,5])/length(sumtable[,5])

# Vbeta credible intervals.
Vbeta_Vec = matrix(Vbeta,nrow=1,ncol=(nvars*nvars))
cat("95% Credible Intervals for Gammadraws",fill=TRUE)
credint = matrix(nrow=(nvars*nvars),ncol=3); R=Mcmc$R/Mcmc$keep
for (i in 1:(nvars*nvars)) {
  credint[i,1] = quantile(outtest$Vbetadraw[round(2*(R/3)):R,i],.025)
  credint[i,2] = quantile(outtest$Vbetadraw[round(2*(R/3)):R,i],.975)
  if ((as.numeric(credint[i,1]) <= Vbeta_Vec[i])&(Vbeta_Vec[i] <= as.numeric(credint[i,2]))) credint[i,3] = 1
  if ((as.numeric(credint[i,1]) > Vbeta_Vec[i])|(Vbeta_Vec[i] > as.numeric(credint[i,2]))) credint[i,3] = 0
}
sumtable = cbind(matrix(cbind(Vbeta_Vec,t(apply(outtest$Vbetadraw[round(2*(R/3)):R,],2,mean))),ncol=2),credint)
colnames(sumtable) = c("True","Mean","Lower","Upper","In"); sumtable
cat("Proportion of Estimates Within the Interval",fill=TRUE); sum(sumtable[,5])/length(sumtable[,5])

# Separate charts for each covariate.
par(mfrow=c(3,3))
for (i in 1:(nvars*ncovs)) {
  matplot(outtest$Gammadraw[,i],type="l",ylim=c(min(outtest$Gammadraw),max(outtest$Gammadraw)),ylab="Gamma Draw",col=2)
  abline(h=Gamma_Vec[i],col=1)
}

# Specify the post-burn-in iterations to use.
start_point = 800; end_point = 1000

# Log marginal density.
logMargDenNR(outtest$llikedraw[start_point:end_point])

# Save testrun output.
testrun = list(data=Data,outtest=outtest)

save(testrun,file="Overfit TestRun04 Excluding Some Two-Way Interactions.RData")

# Continue from an existing run.
Mcmc = list(R=5000,keep=5,step=.08,sim_ind=1,cont_ind=1)

source("Estimation/1 Hierarchical MNL/hier_mnl.R"); outtest_cont = hier_mnl(Data,Prior,Mcmc,Cont=outtest$Cont)

# Load testrun output.
load(file="TestRun01.RData")
Data <- testrun$data; outtest <- testrun$outtest
Beta <- Data$Beta; Gamma <- Data$Gamma; Vbeta <- Data$Vbeta
nresp = length(Data$data); nscns = length(Data$data[[1]]$yy); nalts = length(Data$data[[1]]$XX[,1])/nscns
nvars = ncol(Data$data[[1]]$X); ncovs = ncol(Data$Z)


# Estimation --------------------------------------------------------------
nresp = length(choice_data)                  # Number of respondents.
nscns = length(choice_data[[1]]$y)           # Number of choice tasks.
nalts = length(choice_data[[1]]$X[,1])/nscns # Number of alternatives in each choice task.
nvars = ncol(choice_data[[1]]$X)             # Number of attribute levels.
ncovs = ncol(covariates$attitudes)+1         # Number of covariates for the upper level.

# Mean-centered covariates with an intercept for the distribution of heterogeneity.
Z = covariates$attitudes
Z_mean = apply(Z,2,mean)
for (i in 1:ncol(Z)) {
  Z[,i] = Z[,i] - Z_mean[i]
}
Z = cbind(rep(1,nrow(Z)),Z)

# Estimate the model.
Data = list(data=choice_data,Z=Z)
Prior = list(gammabar=matrix(rep(0,ncovs*nvars),ncol=nvars),Agamma=0.01*diag(ncovs),nu=nvars+3,V=(nvars+3)*diag(nvars))
Mcmc = list(R=50000,keep=50,step=.08,sim_ind=0,cont_ind=0)

source("Estimation/1 Hierarchical MNL/hier_mnl.R"); out = hier_mnl(Data,Prior,Mcmc)

# # Censor respondents who are responding at random (llike < 0.35).
# betadraw = out$betadraw; loglike = double(nresp)
# burnin = 800; lli = double(dim(betadraw)[3]-burnin)
# for (resp in 1:nresp) {
#   bstari = betadraw[resp,,]
#   for (rep in (burnin+1):ncol(bstari)) {
#     lli[rep-burnin] = llmnl(bstari[,rep],y=choice_data[[resp]]$y,X=choice_data[[resp]]$X)
#   }
#   loglike[resp] = mean(lli)
# }
# lowlike = loglike < nscns*log(0.35)
# matrix(lowlike*1,ncol=1)                                # To censor in the Excel sheet directly.
# # save(lowlike,file="Data/Robotic Vacuums/lowlike.RData") # To censor in the Data Setup R script.

# Aggregate-level plot.
par(mfrow=c(3,1))
matplot(out$llikedraw,type="l",main="50k iterations hier_mnl",ylab="Log Likelihood")
matplot(out$Gammadraw,type="l",col=c(1:nvars),ylab="Gamma Draws")
matplot(out$Vbetadraw[,1:nvars],type="l",col=c(1:nvars),ylab="Vbeta Draws")

# Separate charts for each Gamma.
par(mfrow=c(3,3))
for (i in 1:(nvars*ncovs)) {
  matplot(out$Gammadraw[,i],type="l",ylim=c(min(out$Gammadraw),max(out$Gammadraw)),ylab="Gamma Draw",col=2)
}

# Separate charts for each Vbeta.
par(mfrow=c(3,3))
for (i in 1:(nvars*nvars)) {
  matplot(out$Vbetadraw[,i],type="l",ylim=c(min(out$Vbetadraw),max(out$Vbetadraw)),ylab="Vbeta Draw",col=3)
}

# Specify the post-burn-in iterations to use.
start_point = 600; end_point = 1000

# Log marginal density.
logMargDenNR(out$llikedraw[start_point:end_point])

# Gamma posterior estimates.
gamma_hat = NULL
for (i in 1:ncol(out$Gammadraw)) {
  gamma_hat = rbind(gamma_hat,round(mean(out$Gammadraw[start_point:end_point,i]),digits=2))
  print(round(mean(out$Gammadraw[start_point:end_point,i]),digits=2))
}
t(matrix(gamma_hat,ncol=nvars))

gamma_sd = NULL
for (i in 1:ncol(out$Gammadraw)) {
  gamma_sd = rbind(gamma_sd,round(sd(out$Gammadraw[start_point:end_point,i]),digits=2))
  print(round(sd(out$Gammadraw[start_point:end_point,i]),digits=2))
}
t(matrix(gamma_sd,ncol=nvars))

# Vbeta posterior estimates.
Vbeta_hat = NULL
for (i in 1:ncol(out$Vbetadraw)) {
  Vbeta_hat = rbind(Vbeta_hat,round(mean(out$Vbetadraw[start_point:end_point,i]),digits=2))
}
diag(matrix(Vbeta_hat,ncol=nvars))
round(mean(diag(matrix(Vbeta_hat,ncol=nvars))),3); round(sd(diag(matrix(Vbeta_hat,ncol=nvars))),3)

Vbeta_sd = NULL
for (i in 1:ncol(out$Vbetadraw)) {
  Vbeta_sd = rbind(Vbeta_sd,round(sd(out$Vbetadraw[start_point:end_point,i]),digits=2))
}
diag(matrix(Vbeta_sd,ncol=nvars))
round(mean(diag(matrix(Vbeta_sd,ncol=nvars))),3); round(sd(diag(matrix(Vbeta_sd,ncol=nvars))),3)

# Posterior plots of the distribution of heterogeneity.
par(mfrow=c(3,2))
plot(density(out$betadraw[,1,start_point:end_point]),main="Attribute 1, Level 1",xlab="",xlim=c(-10,10),ylim=c(0,.35))

# Save run output.
run = list(Data=Data,Prior=Prior,Mcmc=Mcmc,out=out)
save(run,file="Run02.RData")

# Continue from an existing run.
Mcmc = list(R=20000,keep=20,step=.08,sim_ind=0,mis_ind=mis_ind,cont_ind=1)
source("Estimation/1 Hierarchical MNL/hier_mnl.R")
out_cont = hier_mnl(Data,Prior,Mcmc,Cont=out$Cont)

# Save continued run output.
save(out_cont,file="Run02_Cont02 (40k).RData")

# Load run output.
load(file="Run02 HMNL with Covariates (20k hier_mnl).RData")
Data <- run$Data; Prior <- run$Prior; out <- run$out
nresp = length(Data$data); nscns = length(Data$data[[1]]$yy); nalts = length(Data$data[[1]]$XX[,1])/nscns
nvars = ncol(Data$data[[1]]$X); ncovs = ncol(Data$Z)
