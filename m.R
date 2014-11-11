#################################################################################################
# Arcidiacono and Miller (2011) coding assignment
# Matt Beamer & Cheng-Yu Chan
# 11/08/2014
#################################################################################################

source('header.R')

# Global parameters. Will not be passing into functions.
N = 10^6
theta_1 = 10
theta_2 = 1

s_1 = 1
s_2 = 4
pi_s = .25
beta = .99

eta = 10^-5 #For calculating lower bound on x
x_m = max(x_bound(eta = eta, s = s_1), x_bound(eta = eta, s = s_2))

set.seed(1023)
epsilon <- data.frame(eps = rgev(n = N, xi = 0, mu = 0, beta = 1))

u <- values()

# Initialize CCPs
p <- 
  matrix(rep(0.5,(x_m + 1) * 4),ncol = 4) %>%
  as.data.frame
phat <- 
  matrix(rep(0,(x_m + 1) * 4),ncol = 4) %>%
  as.data.frame

# Or, if you want something more random
# p <- data.frame(p0s1 = runif(53),p0s2 = runif(53)) %>% mutate(p1s1 = 1-p0s1,p1s2 = 1-p0s2))

# Need a better while loop test. Only using first element right now.
while(abs(max(phat - p)) > 10^(-16)){
  phat <- p

  # Apply the F operator to get the difference in value between keeping the engine and replacing it
  vals <- Foperator(P=phat)

  # Apply the lambda operator to get the updated CCPs
  p <- lambda(v_bar = vals)
}















