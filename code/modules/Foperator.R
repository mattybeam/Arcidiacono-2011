# Operator F 

# F: CCPs -> values
# INPUT: P = CCPs, P is an (x_m + 2) by 4 matrix with p0(x,s) and p1(x,s) for all x = 0,1,...x_m + 1 and s = s_1, s_2, where
#                 column 1 is p0(x,s_1), the CCP of not replacing the engine of brand s_1 at mileage x 
#                 column 2 is p0(x,s_2), the CCP of not replacing the engine of brand s_2 at mileage x 
#                 column 3 is p1(x,s_1), the CCP of replacing the engine of brand s_1 at mileage x
#                 column 4 is p1(x,s_2), the CCP of replacing the engine at of brand s_2 mileage x 
#                   Note: p1(x,s) = 1 for s = s_1, s_2
# OUTPUT: f(x,s | P) = v1(x,s) - v0(x,s), the difference in choice specific values between keeping the engine and replacing
#                               the engine of brand s at mileage x, for x = 0,1,...,x_m, given P
#             Note: This is an (x_m + 1) by 2 matrix, where
#                       column 1 is f(x,s_1 | P), the difference for engine brand s_1
#                       column 2 is f(x,s_2 | P), the difference for engine brand s_2

Foperator <- function(P){
  data.frame(
    f_s1 = u[, 1] - u[, 3] + beta * log(P[1, 3]) - beta * log(P[2:nrow(P), 3]),
    f_s2 = u[, 2] - u[, 4] + beta * log(P[1, 4]) - beta * log(P[2:nrow(P), 4])
  )
}
