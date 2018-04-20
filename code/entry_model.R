#--------------------------------------------#
# R CODE FOR THE ENTRY MODEL (BERRY, 1992)
# Mai-Anh Dang, Hoang Tran, Xiahua Wang
# M2 EEE - Toulouse School of Economics
# April, 2018
#--------------------------------------------#
#
# This file include:
#
# Simple Homogeneous Firm Model 
# Berry (1992) Entry model with 3 identification strategies
#
# Data
# Provided by Prof. Bontemps, with the socioeconomics from Census
# and the airlines data from DB1B, Department of Transportation
#---------------------------------------------#


## (1) Load Data ####

dat <-read.csv("./data/markets_final.csv",header=T)

#--------------------------------------------#
# Model: Simulated MLE ####
#--------------------------------------------#

nfirm = dat$firm_nb

# Covariates for 6 firms
nmkts = nrow(dat); # Number of markets
ints = rep(1,nmkts);  # intercepts
nfirm = 6

# transform
dat$airpresence = log(dat$airpresence)
dat$population = log(dat$population)
dat$distance = log(dat$distance)
dat$distancesq = log(dat$distancesq)

# no heterogeneity matrix
AAmat = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route)
DLmat = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route)
UAmat = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route)
ALmat = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route)
WNmat = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route)
LCCmat = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route)

# heterogeneity matrix
AAmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlineaa)
DLmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlinedl)
UAmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlineua)
ALmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlineal)
WNmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlinewn)
LCCmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlinelcc)

# number of parameters:
k.par = ncol(AAmat)


# simulate for epsilon_{i,m,nreps}
set.seed(1234)
nreps = 1 # Number of Simulations
u.mo = matrix(rnorm(nreps*nmkts),nmkts,nreps) # characteristics of the market

# market x firms, u.mk
u.aa = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.dl = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.ua = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.al = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.wn = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.lcc = matrix(rnorm(nreps*nmkts), nmkts, nreps) 


# Create Dependent Vectors
nofirm = (1-dat$airlineaa)*(1-dat$airlinedl)*(1-dat$airlineal)*(1-dat$airlineua)*(1-dat$airlineal)*(1-dat$airlinelcc)
monoDL = (1-dat$airlineaa)*dat$airlinedl*(1-dat$airlineal)*(1-dat$airlineua)*(1-dat$airlineal)*(1-dat$airlinelcc)
monoAL = (1-dat$airlineaa)*(1-dat$airlinedl)*dat$airlineal*(1-dat$airlineua)*(1-dat$airlineal)*(1-dat$airlinelcc)
mono = monoDL + monoAL
duo = 1 - nofirm - monoDL - monoAL


# Objective function definition

Berry.Obj <- function(theta)
{
  # move.rule = { DL first, AL first, most profitable first }
  
  # set parameters
  if (model == "no heter"){
    k.par = ncol(AAmat)
  } else {
    k.par = ncol(AAmat.heter)
  }
  
  beta = theta[1:k.par];
  delta = exp(theta[k.par+1]); # We exponentiate to ensure competitive effects are negative
  if (model == "simulated"){
    rho = theta[k.par+2]
  }
  
  
  # Deterministic component of profits (param from 1 to 7)
  
  if (model == "no heter"){
    pi.AA = AAmat%*%beta + u.mo;
    pi.DL = DLmat%*%beta + u.mo;
    pi.UA = UAmat%*%beta + u.mo;
    pi.AL = ALmat%*%beta + u.mo;
    pi.WN = WNmat%*%beta + u.mo;
    pi.LCC = LCCmat%*%beta + u.mo;
  }
  
  if (model == "heter"){
    pi.AA = AAmat.heter%*%beta + u.mo;
    pi.DL = DLmat.heter%*%beta + u.mo;
    pi.UA = UAmat.heter%*%beta + u.mo;
    pi.AL = ALmat.heter%*%beta + u.mo;
    pi.WN = WNmat.heter%*%beta + u.mo;
    pi.LCC = LCCmat.heter%*%beta + u.mo;
  }
  
  if (model == "no cor"){
    pi.AA = AAmat.heter%*%beta + u.aa;
    pi.DL = DLmat.heter%*%beta + u.dl;
    pi.UA = UAmat.heter%*%beta + u.ua;
    pi.AL = ALmat.heter%*%beta + u.al;
    pi.WN = WNmat.heter%*%beta + u.wn;
    pi.LCC = LCCmat.heter%*%beta + u.lcc;
  }
  
  if (model == "simulated"){
    pi.AA = AAmat.heter%*%beta + u.aa*sqrt(1-rho) + u.mo*rho;
    pi.DL = DLmat.heter%*%beta + u.dl*sqrt(1-rho) + u.mo*rho;
    pi.UA = UAmat.heter%*%beta + u.ua*sqrt(1-rho) + u.mo*rho;
    pi.AL = ALmat.heter%*%beta + u.al*sqrt(1-rho) + u.mo*rho;
    pi.WN = WNmat.heter%*%beta + u.wn*sqrt(1-rho) + u.mo*rho;
    pi.LCC = LCCmat.heter%*%beta + u.lcc*sqrt(1-rho) + u.mo*rho;
  }
  
  # We use analytical probabilities 
  # Entry under assumption that DL moves first
  if(move.rule == "DL") {	
    # no firm enter
    p0.an = pnorm(-pi.AA)*pnorm(-pi.DL)*pnorm(-pi.UA)*pnorm(-pi.AL)*pnorm(-pi.WN)*pnorm(-pi.LCC)
    p1.an = pnorm(-pi.AA+delta)*pnorm(pi.DL)*pnorm(-pi.UA+delta)*pnorm(-pi.AL+delta)*pnorm(-pi.WN+delta)*pnorm(-pi.LCC+delta)
    p2.an = 1- p0.an - p1.an;
  }
  # Entry under assumption that AL moves first
  if(move.rule == "AL") {  
    p0.an = pnorm(-pi.AA)*pnorm(-pi.DL)*pnorm(-pi.UA)*pnorm(-pi.AL)*pnorm(-pi.WN)*pnorm(-pi.LCC)
    p1.an = pnorm(-pi.AA+delta)*pnorm(-pi.DL+delta)*pnorm(-pi.UA+delta)*pnorm(pi.AL)*pnorm(-pi.WN+delta)*pnorm(-pi.LCC+delta)
    p2.an = 1- p0.an - p1.an;
  }
  # Entry under assumption that the more profitable firm (between DL and AL) moves first
  if(move.rule == "profit") { 
    p0.an = pnorm(-pi.AA)*pnorm(-pi.DL)*pnorm(-pi.UA)*pnorm(-pi.AL)*pnorm(-pi.WN)*pnorm(-pi.LCC)
    p1.an = pnorm(pi.DL)*pnorm(-pi.AL + delta) - (pnorm(-pi.DL+delta) - pnorm(-pi.DL))*(pnorm(-pi.AL+delta) - pnorm(-pi.AL))*(1 - pnorm((pi.AL-pi.DL)/2))
    p2.an = 1- p0.an - p1.an 
  }	
  
  # Construct -LogLikelihood
  # We use maximum Likelihood to ensure that the comparison to incomplete information  is fair
  # Analytical Probabilities
  p.sim.duo = p2.an
  p.sim.mono = p1.an
  p.sim.0 = p0.an
  
  # Check for numerical issues
  p.sim.duo[p.sim.duo<=0] = 1E-10 # to avoid the negative values
  p.sim.mono[p.sim.mono<=0] = 1E-10
  p.sim.0[p.sim.0<=0] = 1E-10
  
  # Log Likelihood
  llik = sum(sum(nofirm*log(p.sim.0) + mono*log(p.sim.mono) + duo*log(p.sim.duo)))		
  
  # Return Value
  -llik
  
}


# DL moves first
model = "simulated" # "no heter", "heter", "no cor"

if (model == "no heter"){
  k.start = ncol(AAmat)
} else if (model == "simulated") {
  k.start = ncol(AAmat.heter) + 1 # add the rho
} else {
  k.start = ncol(AAmat.heter)
}

theta.start = rep(0, k.start+1) # set the starting value

move.rule = "profit"
Berry.profit.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# Notice: the results in other
#----------------------------
# constant
# pop
# dis
# dissq
# route
# city2
# delta (need to take ln of positive)
# delta

move.rule = "AL"
Berry.AL.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

move.rule = "DL"
Berry.DL.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# NOTICE: The delta in the par is (-delta)
# Standard Errors
Berry.profit.se = sqrt(abs(diag(solve(Berry.profit.res$hess))))
Berry.AL.se = sqrt(abs(diag(solve(Berry.AL.res$hess))))
Berry.DL.se = sqrt(abs(diag(solve(Berry.DL.res$hess))))


