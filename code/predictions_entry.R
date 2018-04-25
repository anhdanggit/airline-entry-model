source("entry_simulated.R")
# source("entry_mle_simplified.R")
# input the value of theta.hat here
# theta.hat = 

#-------------------------------#
# simulate for epsilon_{i,m}
nreps = 1 
u.mo = matrix(rnorm(nreps*nmkts),nmkts,nreps) # characteristics of the market

# market x firms, u.ik
u.aa = matrix(rnorm(nreps*nmkts), nmkts, nreps) # marktes in rows and simulation i column
u.dl = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.ua = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.al = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.wn = matrix(rnorm(nreps*nmkts), nmkts, nreps) 
u.lcc = matrix(rnorm(nreps*nmkts), nmkts, nreps)
# heterogeneity matrix (MODIFY THE VAR FOR COUNTERFACTUAL)
AAmat.heter = cbind(ints, dat$population, dat$distance*0.5, dat$distancesq*0.25, dat$routeaa, dat$lairpresenceaa,dat$airlineaa)
DLmat.heter = cbind(ints, dat$population, dat$distance*0.5, dat$distancesq*0.25, dat$routedl, dat$lairpresencedl, dat$airlinedl)
UAmat.heter = cbind(ints, dat$population, dat$distance*0.5, dat$distancesq*0.25, dat$routeua, dat$lairpresenceua, dat$airlineua)
ALmat.heter = cbind(ints, dat$population, dat$distance*0.5, dat$distancesq*0.25, dat$routeal, dat$lairpresenceal, dat$airlineal)
WNmat.heter = cbind(ints, dat$population, dat$distance*0.5, dat$distancesq*0.25, dat$routewn, dat$lairpresencewn, dat$airlinewn)
LCCmat.heter = cbind(ints, dat$population, dat$distance*0.5, dat$distancesq*0.25, dat$routelcc, dat$lairpresencelcc, dat$airlinelcc)




# Predict number of firms by model
beta = theta.hat[1:7]
delta = log(abs(theta.hat[8]))
rho = theta.hat[9]

pi.AA = AAmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.aa
pi.AA[pi.AA == Inf] = 0
predict.AA = rep(0, length(pi.AA))
predict.AA[pi.AA > 0] = 1

pi.DL = DLmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.dl
#pi.DL[pi.DL == Inf] = 0
predict.DL = rep(0, length(pi.DL))
predict.DL[pi.DL > 0] = 1

pi.UA = UAmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.ua
#pi.UA[pi.UA == Inf] = 0
predict.UA = rep(0, length(pi.UA))
predict.UA[pi.UA > 0] = 1

pi.AL = ALmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.al
#pi.AL[pi.AL == Inf] = 0
predict.AL = rep(0, length(pi.AL))
predict.AL[pi.AL > 0] = 1

pi.UA = UAmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.ua
#pi.UA[pi.UA == Inf] = 0
predict.UA = rep(0, length(pi.UA))
predict.UA[pi.UA > 0] = 1

pi.WN = WNmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.al
#pi.WN[pi.WN == Inf] = 0
predict.WN = rep(0, length(pi.WN))
predict.WN[pi.WN > 0] = 1

pi.LCC = LCCmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(abs(1-rho^2))*u.al
#pi.LCC[pi.LCC == Inf] = 0
predict.LCC = rep(0, length(pi.LCC))
predict.LCC[pi.LCC > 0] = 1

predict.firm_nb = predict.AA + predict.DL + predict.UA + predict.AL + predict.WN + predict.LCC
mean(predict.firm_nb)

# % correctly predicted entrants
per1 = mean(predict.AA == dat$airlineaa)
per2 = mean(predict.DL == dat$airlinedl)
per3 = mean(predict.UA == dat$airlineua)
per4 = mean(predict.AL == dat$airlineal)
per5 = mean(predict.WN == dat$airlinewn)
per6 = mean(predict.LCC == dat$airlinelcc)

(per1 + per2 + per3 + per4 + per5 + per6)/6

# mean square error
sqrt(sum((predict.firm_nb - dat$firm_nb)^2)/length(dat$firm_nb))
