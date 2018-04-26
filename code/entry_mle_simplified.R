#--------------------------------------------#
# ENTRY MODEL - MLE (3 Outcomes)
# Mai-Anh Dang, Hoang Tran, Xiahua Wang
# M2 EEE - Toulouse School of Economics
# April, 2018
#--------------------------------------------#
#
# This file include:
#
# Berry (1992) Entry model by MLE
# 3 outcomes: N = 0, N=1, N >=2
#
# Data
# Provided by Prof. Bontemps, with the socioeconomics from Census
# and the airlines data from DB1B, Department of Transportation
#---------------------------------------------#


## (1) Load Data ####

dat <-read.csv("./data/markets_wide_new.csv",header=T)

#--------------------------------------------#
# Model: MLE (with 3 outcomes of market)  ####
#--------------------------------------------#

nfirm = dat$firm_nb

# Covariates for 6 firms
nmkts = nrow(dat); # Number of markets
ints = rep(1,nmkts);  # intercepts
nfirm = 6

# transform
dat$population = log(dat$population)
dat$distance = log(dat$distance)
dat$distancesq = log(dat$distancesq)

# no heterogeneity matrix
AAmat = cbind(ints, dat$population, dat$distance, dat$distancesq)
DLmat = cbind(ints, dat$population, dat$distance, dat$distancesq)
UAmat = cbind(ints, dat$population, dat$distance, dat$distancesq)
ALmat = cbind(ints, dat$population, dat$distance, dat$distancesq)
WNmat = cbind(ints, dat$population, dat$distance, dat$distancesq)
LCCmat = cbind(ints, dat$population, dat$distance, dat$distancesq)

# heterogeneity matrix
AAmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$routeaa, dat$lairpresenceaa,dat$airlineaa)
DLmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$routedl, dat$lairpresencedl, dat$airlinedl)
UAmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$routeua, dat$lairpresenceua, dat$airlineua)
ALmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$routeal, dat$lairpresenceal, dat$airlineal)
WNmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$routewn, dat$lairpresencewn, dat$airlinewn)
LCCmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$routelcc, dat$lairpresencelcc, dat$airlinelcc)

# number of parameters:
k.par = ncol(AAmat)


# Create Dependent Vectors as dummies (3 outcomes, N=0, N=1, N>=2)
nofirm = rep(0, length(dat$market))
nofirm[dat$firm_nb==0]=1

mono = rep(0, length(dat$market))
mono[dat$firm_ng==1]=1

duo = rep(0, length(dat$market))
duo = 1 - mono - nofirm


# Objective function definition
Berry.Obj <- function(theta)
{
  
  # set parameters
  if (model == "no heter"){
    k.par = ncol(AAmat)
  } else {
    k.par = ncol(AAmat.heter)
  }
  
  beta = theta[1:k.par];
  delta = exp(theta[k.par+1]); # We exponentiate to ensure competitive effects are negative
  
  
  # Deterministic component of profits (param from 1 to 7)
  
  if (model == "no heter"){
    # Model 1: pi = X*beta + u_{m,0}
    pi.AA = AAmat%*%beta;
    pi.DL = DLmat%*%beta;
    pi.UA = UAmat%*%beta;
    pi.AL = ALmat%*%beta;
    pi.WN = WNmat%*%beta;
    pi.LCC = LCCmat%*%beta;
  }
  
  if (model == "heter"){
    # Model 2: pi = X_{market}*beta + Z_{market, firm}*alpha + u_{m,0}
    pi.AA = AAmat.heter%*%beta;
    pi.DL = DLmat.heter%*%beta;
    pi.UA = UAmat.heter%*%beta;
    pi.AL = ALmat.heter%*%beta;
    pi.WN = WNmat.heter%*%beta;
    pi.LCC = LCCmat.heter%*%beta;
  }
  
  if (model == "no cor"){
    # Model 3: pi = X_{market}*beta + Z_{market, firm}*alpha + u_{m,i}
    # simulate for epsilon_{i,m}
    nreps = 100 
    
    # market x firms, u.ik
    u.aa = matrix(rnorm(nreps*nmkts), nmkts, nreps) # randomness for markets, by each firms
    u.dl = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
    u.ua = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
    u.al = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
    u.wn = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
    u.lcc = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
    
    pi.AA = AAmat.heter%*%beta + mean(u.aa); 
    pi.DL = DLmat.heter%*%beta + mean(u.dl);
    pi.UA = UAmat.heter%*%beta + mean(u.ua);
    pi.AL = ALmat.heter%*%beta + mean(u.al);
    pi.WN = WNmat.heter%*%beta + mean(u.wn);
    pi.LCC = LCCmat.heter%*%beta + mean(u.lcc);
  }
  
  
  # We use analytical probabilities 
  # Entry under assumption that more market presence moves first
  if(move.rule == "market") {  
    p0.an = pnorm(-pi.AA)*pnorm(-pi.DL)*pnorm(-pi.UA)*pnorm(-pi.AL)*pnorm(-pi.WN)*pnorm(-pi.LCC)
    p1.an = pnorm(pi.DL)*pnorm(-pi.AL + delta) - (pnorm(-pi.DL+delta) - pnorm(-pi.DL))*(pnorm(-pi.AL+delta) - pnorm(-pi.AL))*(1 - pnorm((pi.AL-pi.DL)/2))
    p2.an = 1- p0.an - p1.an;
  }
  # Entry under assumption that the more profitable firm (between DL and AL) moves first
  if(move.rule == "profit") { 
    
    p0.an = rep(0, length(pi.AA))
    p1.an = rep(0, length(pi.AA))
    p2.an = rep(0, length(pi.AA))
    
    p0.an = pnorm(-pi.AA)*pnorm(-pi.DL)*pnorm(-pi.UA)*pnorm(-pi.AL)*pnorm(-pi.WN)*pnorm(-pi.LCC)
    
    for (i in 1:length(pi.AA)){
      # for each market rank the profit
      list = sort(c(pi.AA[i], pi.DL[i], pi.UA[i], pi.AL[i], pi.WN[i], pi.LCC[i]), decreasing = TRUE)
      p1.an = pnorm(list[1])*pnorm(-list[1] + delta) - (pnorm(-list[1]+delta) - pnorm(-list[1]))*(pnorm(-list[2]+delta) - pnorm(-list[2]))*(1 - pnorm((list[2]-list[1])/2))
    }
    
    p2.an = 1- p0.an - p1.an 

  }	
  
  # Construct -LogLikelihood
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


#---- Variations of Estimates -----------#
model = "no cor" # "no heter", "heter", "no cor"
move.rule = "profit" # "market", "profit"
#----------------------------------------#

if (model == "no heter"){
  k.start = ncol(AAmat)
} else if (model == "simulated") {
  k.start = ncol(AAmat.heter) + 1 # add the rho
} else {
  k.start = ncol(AAmat.heter)
}

theta.start = rep(0, k.start+1) # set the starting value

Berry.profit.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)
theta_hat = Berry.profit.res$par
theta_se = sqrt(abs(diag(solve(Berry.profit.res$hess))))


colMeans(theta_hat)
colMeans(theta_se)