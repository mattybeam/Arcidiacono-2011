# For N engines, with pi_s * N of them of type s_1 and (1 - pi_s) of them of type s_2, find the time until each engine is 
# new again (replaced in the last period). Store the results in a data frame with three columns:
#   Column 1: engine number, from 1 to N
#   Column 2: type, 1 for s_1 and 2 for s_2
#   Column 3: mileage at replacement, x

todeath <- function(type){
  laply(1:x_m,function(l) rbinom(1,1,p[l,type])) %>%
    which.min
}

engines <- data.frame(
  engine_number = 1:N,
  type = rbinom(N,1,1 - pi_s),
  x0 <- rep(0,N)
  ) %>%
  mutate(type = type + 1)

engines$x <- apply(engines,1,function(y) todeath(y[2]))
engines <- mutate(engines, x = x - 1)
