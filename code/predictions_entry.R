source("entry_model.R")

#---- Variations of Estimates -----------#
model = "simulated" # "no heter", "heter", "no cor", "simulated
move.rule = "profit" # "market", "profit"
S = 100 # number of simulation
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

for (i in 1:S){
  Berry.profit.res = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)
  theta_hat[i,] = Berry.profit.res$par
  theta_se[i,] = sqrt(abs(diag(solve(Berry.profit.res$hess))))
}

theta.hat = colMeans(theta_hat)
colMeans(theta_se)

#-------------------------------#

# heterogeneity matrix (MODIFY THE VAR FOR COUNTERFACTUAL)
AAmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlineaa)
DLmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlinedl)
UAmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlineua)
ALmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlineal)
WNmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlinewn)
LCCmat.heter = cbind(ints, dat$population, dat$distance, dat$distancesq, dat$route, dat$airlinelcc)



# Predict number of firms by model
beta = theta.hat[1:6]
delta = log(abs(theta.hat[7]))
rho = theta.hat[8]

pi.AA = AAmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.aa
pi.AA[pi.AA == Inf] = 0
predict.AA = rep(0, length(pi.AA))
predict.AA[pi.AA > 0] = 1

pi.DL = DLmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.dl
#pi.DL[pi.DL == Inf] = 0
predict.DL = rep(0, length(pi.DL))
predict.DL[pi.DL > 0] = 1

pi.UA = UAmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.ua
#pi.UA[pi.UA == Inf] = 0
predict.UA = rep(0, length(pi.UA))
predict.UA[pi.UA > 0] = 1

pi.AL = ALmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.al
#pi.AL[pi.AL == Inf] = 0
predict.AL = rep(0, length(pi.AL))
predict.AL[pi.AL > 0] = 1

pi.UA = UAmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.ua
#pi.UA[pi.UA == Inf] = 0
predict.UA = rep(0, length(pi.UA))
predict.UA[pi.UA > 0] = 1

pi.WN = WNmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.al
#pi.WN[pi.WN == Inf] = 0
predict.WN = rep(0, length(pi.WN))
predict.WN[pi.WN > 0] = 1

pi.LCC = LCCmat.heter%*%beta - delta*log(dat$firm_nb) + rho*u.mo + sqrt(1-rho^2)*u.al
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
