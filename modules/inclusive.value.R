# Evaluate the inclusive value for arbitrarily large values
# INPUT:  
#     v1 = deterministic utility of option 1
#     v2 = deterministic utility of option 2
# OUTPUT:
#     log(exp(v1) + exp(v2)) + gamma, the inclusive value, where gamma is Euler's constant (-digamma(1))

inclusive.value <- function(v1,v2){
  log(1 + exp(v2 - v1)) + v1 + -digamma(1)
}
