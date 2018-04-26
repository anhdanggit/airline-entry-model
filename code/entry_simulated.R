#--------------------------------------------#
# THE ENTRY MODEL - SMLE
# Mai-Anh Dang, Hoang Tran, Xiahua Wang
# M2 EEE - Toulouse School of Economics
# April, 2018
#--------------------------------------------#
#
# This file include:
# Entry Model estimated by simulattion method
# by 6 firms, N = 1,2,3,4,5,6
#
# Data
# Provided by Prof. Bontemps, with the socioeconomics from Census
# and the airlines data from DB1B, Department of Transportation
#---------------------------------------------#


## (1) Load Data ####

dat <-read.csv("./data/markets_wide_new.csv",header=T)

#--------------------------------------------#
# Model: Simulated MLE ####
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


# Create Dependent Vectors (actual number of firm), dummy
nfirm0 = rep(0, length(dat$market))
nfirm0[dat$firm_nb==0]=1 # =1 if the actual nfirm = 0

nfirm1 = rep(0, length(dat$market))
nfirm1[dat$firm_nb==1]=1

nfirm2 = rep(0, length(dat$market))
nfirm2[dat$firm_nb==2]=1

nfirm3 = rep(0, length(dat$market))
nfirm3[dat$firm_nb==3]=1

nfirm4 = rep(0, length(dat$market))
nfirm4[dat$firm_nb==4]=1

nfirm5 = rep(0, length(dat$market))
nfirm5[dat$firm_nb==5]=1

nfirm6 = rep(0, length(dat$market))
nfirm6[dat$firm_nb==6]=1


# Objective function definition

Berry.Obj <- function(theta)
{
  # simulate for epsilon_{i,m}
  nreps = 1 
  u.mo = matrix(rnorm(nreps*nmkts),nmkts,nreps) # randomness by market
  
  # market x firms, u.ik
  u.aa = matrix(rnorm(nreps*nmkts), nmkts, nreps) # marktes in rows and simulation i column
  u.dl = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
  u.ua = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
  u.al = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
  u.wn = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
  u.lcc = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
  
  # set parameters
  k.par = ncol(AAmat.heter)
  beta = theta[1:k.par];
  delta = theta[k.par+1]; 
  rho = theta[k.par+2]
  
  
  # Deterministic component of profits (param from 1 to 7)
  if (model == "simulated"){
    pi.AA = AAmat.heter%*%beta + u.aa*sqrt(1-rho) + u.mo*rho;
    pi.DL = DLmat.heter%*%beta + u.dl*sqrt(1-rho) + u.mo*rho;
    pi.UA = UAmat.heter%*%beta + u.ua*sqrt(1-rho) + u.mo*rho;
    pi.AL = ALmat.heter%*%beta + u.al*sqrt(1-rho) + u.mo*rho;
    pi.WN = WNmat.heter%*%beta + u.wn*sqrt(1-rho) + u.mo*rho;
    pi.LCC = LCCmat.heter%*%beta + u.lcc*sqrt(1-rho) + u.mo*rho;
  }


  # Simulated MLE by Berry
  if(move.rule == "simulated") { 
    
    N0.an = rep(0, length(pi.AA)) # get dummies 0,1 if the the N-equilibirum is 0,1..6
    N1.an = rep(0, length(pi.AA))
    N2.an = rep(0, length(pi.AA))
    N3.an = rep(0, length(pi.AA))
    N4.an = rep(0, length(pi.AA))
    N5.an = rep(0, length(pi.AA))
    N6.an = rep(0, length(pi.AA))
    
    for (i in 1:length(pi.AA)){
      # for each market rank the profit
      list = sort(c(pi.AA[i], pi.DL[i], pi.UA[i], pi.AL[i], pi.WN[i], pi.LCC[i]), decreasing = TRUE)

      n1 = sum(list >= 0)
      if (n1 == 0){
        N0.an[i] = 1
      } 
      
      n2 = sum(list >= delta*log(2))
      if (n2 < 2){
        N1.an[i] = 1
      }
      
      n3 = sum(list >= delta*log(3))
      if (n3 < 3){
        N2.an[i] = 1
      }
      
      n4 = sum(list >= delta*log(4))
      if (n4 < 4){
        N3.an[i] = 1
      }
      
      n5 = sum(list >= delta*log(5))
      if (n5 < 5){
        N4.an[i] = 1
      }
      
      n6 = sum(list >= delta*log(6))
      if (n6 < 6){
        N5.an[i] = 1
      }

    }
    
    N6.an = 1- N0.an - N1.an - N2.an - N3.an - N4.an - N5.an 
  }	
  
  # Log Likelihood by empirical frequency
  llik = sum(log(sum(N0.an == nfirm0)), log(sum(N1.an = nfirm1)), log(sum(N2.an == nfirm2)), log(sum(N3.an = nfirm3)), log(sum(N4.an == nfirm4)), log(sum(N5.an = nfirm5)), log(sum(N6.an = nfirm6)))
  
  # Return Value
  -llik
  
}


#---- Variations of Estimates -----------#
model = "simulated" # "simulated"
move.rule = "simulated" # "simulated"
S = 1 # number of simulation
#----------------------------------------#

if (model == "no heter"){
  k.start = ncol(AAmat)
} else if (model == "simulated") {
  k.start = ncol(AAmat.heter) + 1 # add the rho
} else {
  k.start = ncol(AAmat.heter)
}

theta.start = rep(0, k.start+1) # set the starting value


theta_hat = matrix(rep(0,S*length(theta.start)), S, length(theta.start))
theta_se = matrix(rep(0,S*length(theta.start)), S, length(theta.start))

# The simulation loops is below:
for (i in 1:S){
  Berry.profit.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)
  theta_hat[i,] = Berry.profit.res$par
  #theta_se[i,] = sqrt(abs(diag(solve(Berry.profit.res$hess))))
}

colMeans(theta_hat)
matrixStats::colSds(theta_hat)

# Notice: the results in other
#----------------------------
# constant
# pop
# dis
# dissq
# route
# lairpresence
# city2
# delta 
# rho





