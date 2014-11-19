# Estimate the primitives, using the function EM.operator, given s and beta
#
# INPUT: s.val, a vector of s values
#        beta, the discount rate
#        est.error, convergence criterion, the max distance between estimation iterations (Default = 10^-3)
#        max.iter, convergence criterion, stops EM algorithm if it gets stuck (Default = 50)
# OUTPUT: thetahat, estimated parameters theta and pi_s

estimatePrimitives <- function(s.val, beta, est.error = 10^-3, max.iter = 50){
  # Randomize starting initial values
  p <- runif(1,min = 0.1,max = 1)
  pi_s <- c(p,1-p)
  theta <- c(runif(1,min = 6,max = 14),runif(1,min = 0.1,max = 2))
  CCP <-  expand.grid(s = s.val,x.t = seq(0,x_m)) %>% 
    mutate(prob.replace = runif(n = (length(s.val) * length(seq(0,x_m)))),
           prob.dont.replace = 1-prob.replace)
  est <- list(CCP = CCP,pi_s = pi_s, theta = theta)
  delta <- est.error + 1
  iter <- 1
  
  # Run EM operator until convergence or max iterations reached
  while(delta > est.error & iter < 50){
    est.next <- try(EM.operator(CCP=est$CCP, est$pi_s, est$theta),silent = TRUE)
    
    # Check if bad initial values. End EM operator and return -Inf if true.
    if ('try-error' %in% class(est.next)){
      est$MLEval = -Inf
      break
    } 
    else {
      delta=max(abs(est$CCP - est.next$CCP),
                abs(est$pi_s - est.next$pi_s),
                abs(est$theta - est.next$theta))
    }
    
    est=est.next
    iter <- iter + 1
  }
  
  return(est)
}
