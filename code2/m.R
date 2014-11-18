#################################################################################################
# Arcidiacono and Miller (2011) coding assignment Part 2
# Matt Beamer & Cheng-Yu Chan
# 11/08/2014
#################################################################################################

source('header.R')
data<- readRDS(paste0(varSave,"engines_data.rds"))
#################################################################################################
# Global parameters. Will not be passing into functions.
#################################################################################################
s.val <- c(1,4)
beta = .99
N = 10^6

x_m <- max(data$replace_Period) + 10

# Initial CCP 
CCP <- expand.grid(s=s.val, x.t=seq(0, x_m), prob.replace = 0.5, prob.dont.replace = 0.5) 
trueCCP<- cbind(c(0:53),readRDS(paste0(varSave,"trueCCP.rds")))
CCP <- left_join(CCP, trueCCP, by = c("x.t" = "c(0:53)")) %>%
  mutate(prob.replace = ifelse(s == s.val[1], f_s1.1, f_s2.1)) %>%
  mutate(prob.dont.replace = ifelse(s == s.val[1], f_s1, f_s2)) %>%
  select(1:4)

theta <- c(11,2)
pi_s = .27
