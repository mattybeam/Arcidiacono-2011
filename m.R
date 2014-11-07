#################################################################################################
# Nested logit coding assignment
# Matt Beamer & Cheng-Yu Chan
# 10/24/2014
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
epsilon <- data.frame(rgev(n = N, xi = 0, mu = 0, beta = 1))
colnames(epsilon) <- "eps"

#test<- ddply(epsilon, "eps", .fun = "lambda")
