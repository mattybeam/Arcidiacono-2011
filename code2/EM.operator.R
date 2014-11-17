# EM.operator 
#
# INPUT: list containing
#        (1) a dataframe of CCPs
#        (2) a pi_s value
#        (3) a list of theta values
# OUTPUT: Estimates in the next iteration, a list containing
#        (1) a dataframe of CCPs
#        (2) a pi_s value
#        (3) a list of theta values

EM.operator <- function(CCP, pi_s, theta){
  # (2.17): compute next iteration of q.s
  q.s <- gamma.Operator(CCP = CCP, theta = theta, beta = beta) %>%
    CCP.to.likelihood %>%
    mutate(pi_s = ifelse(s == s.val[1], pi_s, 1 - pi_s)) %>%
    group_by(s) %>%
    mutate(likelihood = likelihood * pi_s) %>%
    ungroup %>%
    group_by(x.t) %>%
    summarise(q_1 = likelihood[1]/sum(likelihood), q_2 = likelihood[2]/sum(likelihood)) %>%
    ungroup

  
  # (2.18): compute next iteration of pi_s
  pi_s <- last(cumsum(q.s$q_1 * c(data$Total_replaced,rep(0,10))),order_by = q.s$x.t) / N
}