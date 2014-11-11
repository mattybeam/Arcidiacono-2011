# INPUT: the difference in choice specific values between keeping the engine and replacing
#         the engine of brand s at mileage x, x = 0,1,...,x_m
#         Note: This is an (x_m + 1) by 2 matrix, where
#                 column 1 is f(x,s_1 | P), the difference for engine brand s_1
#                 column 2 is f(x,s_2 | P), the difference for engine brand s_2
# OUTPUT: updated conditional choice probabilities
#                 column 1 is p0(x,s_1), the CCP of not replacing the engine of brand s_1 at mileage x 
#                 column 2 is p0(x,s_2), the CCP of not replacing the engine of brand s_2 at mileage x 
#                 column 3 is p1(x,s_1), the CCP of replacing the engine of brand s_1 at mileage x
#                 column 4 is p1(x,s_2), the CCP of replacing the engine at of brand s_2 mileage x 
#             Note: p1(x_m + 1,s) = 1 for s= s_1, s_2

lambda<- function (v_bar){
  data.frame(
    exp(v_bar) / (1 + exp(v_bar)),
    1 / (1 + exp(v_bar))
  ) %>% rbind(c(0,0,1,1))
}
