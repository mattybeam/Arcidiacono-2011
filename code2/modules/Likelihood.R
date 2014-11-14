like<- function(P, t1, t2){
  data.frame(
    l1 = 1 / (1 + exp(t1 * c(0:x_m) + t2 * s_1 + beta * log(P[1,1]) - beta * log(P[2:x_m,1]))),
    l2 = 1 / (1 + exp(t1 * c(0:x_m) + t2 * s_2 + beta * log(P[1,2]) - beta * log(P[2:x_m,2])))
  )
}