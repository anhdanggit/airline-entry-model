##SINGULAR SE

# Objective function definition

Berry.Obj <- function(theta)
{
  # move.rule = { DL first, AL first, most profitable first }
  
  # set parameters
  
  beta = theta[1:k.par];
  delta = exp(theta[k.par+1]); # We exponentiate to ensure competitive effects are negative
  #rho = theta[10]
  
  # Deterministic component of profits (param from 1 to 7)
  
  pi.DL = DLmat%*%beta;
  pi.AL = ALmat%*%beta;
  
  
  # We use analytical probabilities 
  # This will be difficult for more than two players or if richer error structures are involved
  # In which case Monte-Carlo approaches proposed in Brerry may be used	
  # Entry under assumption that WalMart moves first
  if(move.rule == "DL") {	
    p0.an = pnorm(-pi.DL)*pnorm(-pi.AL)
    p2.an = pnorm(pi.DL-delta)*pnorm(pi.AL-delta)
    pDL.an = pnorm(pi.DL)*pnorm(-pi.AL + delta)
    pAL.an = 1- p0.an - p2.an - pDL.an;
  }
  # Entry under assumption that Kmart moves first
  if(move.rule == "AL") {  
    p0.an = pnorm(-pi.AL)*pnorm(-pi.DL)
    p2.an = pnorm(pi.DL-delta)*pnorm(pi.AL-delta)
    pAL.an = pnorm(pi.AL)*pnorm(-pi.DL + delta)
    pDL.an = 1- p0.an - p2.an - pAL.an;
  }
  # Entry under assumption that the more profitable firm moves first
  if(move.rule == "profit") { 
    p0.an = pnorm(-pi.DL)*pnorm(-pi.AL)
    p2.an = pnorm(pi.DL-delta)*pnorm(pi.AL-delta)
    pDL.an = pnorm(pi.DL)*pnorm(-pi.AL + delta) - (pnorm(-pi.DL+delta) - pnorm(-pi.DL))*(pnorm(-pi.AL+delta) - pnorm(-pi.AL))*(1 - pnorm((pi.AL-pi.DL)/2))
    pAL.an = 1- p0.an - p2.an - pDL.an
  }	
  
  # Construct -LogLikelihood
  # We use maximum Likelihood to ensure that the comparison to incomplete information  is fair
  # Analytical Probabilities
  p.sim.2 = p2.an
  p.sim.DL = pDL.an
  p.sim.AL = pAL.an
  p.sim.0 = p0.an
  
  # Check for numerical issues
  p.sim.2[p.sim.2<=0] = 1E-5 # to avoid the negative values
  p.sim.DL[p.sim.DL<=0] = 1E-5
  p.sim.AL[p.sim.AL<=0] = 1E-5
  p.sim.0[p.sim.0<=0] = 1E-5
  
  # Log Likelihood
  llik = sum(sum(nofirm*log(p.sim.0) + duo*log(p.sim.2) + monoDL*log(p.sim.DL) + monoAL*log(p.sim.AL)))		
  
  # Return Value
  -llik
  
}

# Estimation for each equilibrium selection approach
move.rule = "AL"
theta.start = rep(0, 7);
Berry.res1 = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# Estimation for each equilibrium selection approach
move.rule = "DL"
theta.start = rep(0, 7);
Berry.res2 = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)

# Estimation for each equilibrium selection approach
move.rule = "profit"
theta.start = rep(0, 7);
Berry.res3 = optim(theta.start,Berry.Obj,control=list(trace=10,maxit=1000),method="BFGS",hessian=T)


# Standard Errors (NOT WORK: TRY BOOTSTRAP)
Berry.res1.se = sqrt(diag(solve(Berry.res1$hess))) # Singular
Berry.res2.se = sqrt(diag(solve(Berry.res2$hess))) # Singular
Berry.res3.se = sqrt(diag(solve(Berry.res3$hess))) # Singular