#################################################################################################
# Arcidiacono and Miller (2011) coding assignment Part 2
# Matt Beamer & Cheng-Yu Chan
# 11/08/2014
#################################################################################################

source('code2/header.R')
data<- readRDS(paste0(varSave,"engines_data.rds"))
#################################################################################################
# Global parameters. Will not be passing into functions.
#################################################################################################
s.val <- c(1,4)
beta = .99
N = 10^6

x_m <- max(data$replace_Period) + 10

<<<<<<< HEAD
CCP <- expand.grid(s=s.val, x.t=seq(0, x_m), prob.replace=.5, prob.dont.replace=.5) 
=======
# Initial CCP 
CCP <- expand.grid(s=s.val, x.t=seq(0, x_m), prob.replace = 0.5, prob.dont.replace = 0.5) 
>>>>>>> origin/master
theta <- c(10,1)
pi_s = .25


# l<- like(P = phat, t1 = theta_1hat, t2 = theta_2hat)
