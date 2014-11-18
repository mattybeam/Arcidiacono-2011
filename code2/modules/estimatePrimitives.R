# Estimate the primitives, using the function EM.operator, given s and beta
#
# INPUT: s.val, a vector of s values
#        beta, the discount rate
# OUTPUT: thetahat, estimated parameters theta and pi_s

estimatePrimitives <- function(s.val,beta,est.error = 10^-3){
  # Need to figure out how to run at multiple different starting values and take global max of theta likelihood
  pi_s <- runif(1,min = 0,max = 0.5)
  theta <- c(runif(1,min = 6,max = 14),runif(1,min = 0,max = 2))
  CCP <- expand.grid(s = s.val,x.t = seq(0,x_m)) %>% mutate(prob.replace = runif(nrow(CCP)),prob.dont.replace = 1-prob.replace)
  (est <- list(CCP = CCP,pi_s = pi_s, theta = theta))
  delta <- est.error + 1
  while(delta > est.error){
    est.next=EM.operator(CCP=est$CCP, est$pi_s, est$theta)
    delta=max(abs(est$CCP - est.next$CCP),abs(est$pi_s - est.next$pi_s),abs(est$theta - est.next$theta))
    est=est.next
  }
}
