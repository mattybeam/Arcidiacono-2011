# Maximum likelihood function to solve over.
# First parameter t are the parameters to solve for (using optim)
MLE <- function(t, c, P, q){
  gamma.Operator(CCP = P, theta = t, beta = beta) %>%
    CCP.to.likelihood %>% 
    merge(y = c, by.x = "x.t", by.y = "replace_Period", all.x = TRUE) %>% 
    mutate(Total_replaced = ifelse(is.na(Total_replaced), 0, Total_replaced)) %>% 
    merge(y = q, by = "x.t") %>% 
    mutate(q.s = ifelse(s == s.val[1], q_1, q_2)) %>% 
    mutate(logl = -1 * Total_replaced * log(likelihood^q.s)) %>% 
    select(logl) %>%
    sum
}


# EM.operator 
#
# INPUT: list containing
#        (1) a dataframe of CCPs, with prob.replace and prob.dont.replace
#        (2) a pi_s value
#        (3) a list of theta values
# OUTPUT: Estimates in the next iteration, a list containing
#        (1) a dataframe of CCPs, with prob.replace and prob.dont.replace
#        (2) a pi_s value
#        (3) a list of theta values

EM.operator <- function(CCP, pi_s, theta){
  # (2.17): compute next iteration of q.s
  q.s <- CCP.to.likelihood(CCP) %>%
    mutate(pi_s = ifelse(s == s.val[1], pi_s, 1 - pi_s)) %>%
    group_by(s) %>%
    mutate(likelihood = likelihood * pi_s) %>%
    ungroup %>%
    group_by(x.t) %>%
    summarise(q_1 = likelihood[1]/sum(likelihood), q_2 = likelihood[2]/sum(likelihood)) %>%
    ungroup

  
  # (2.18): compute next iteration of pi_s
  pi_s <- last(cumsum(q.s$q_1 * c(data$Total_replaced,rep(0,10))),order_by = q.s$x.t) / N
  
  # (2.22): update CCPs
  CCP <- gamma.Operator(CCP = CCP, theta = theta, beta = beta) 
  
  # (2.20): solving for optimal thetas
  results <- optim(theta, MLE, 
                 c = data, P = CCP, q = q.s, 
                 method = 'L-BFGS-B', lower = c(0,0)) # control$fnscale = -1 turns it into a maximization problem
  theta <- results$par
  est <- list(CCP = CCP,pi_s = pi_s,theta = theta)
}











