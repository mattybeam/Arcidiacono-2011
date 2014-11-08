# Calculate deterministic part of utilites for keeping a bus engine and replacing it at each mileage x and type s
#
# INPUT: (automatic) s = bus engine brand values
#                    x = vector from mileage 0 to mileage x_m, the max mileage before the engine would be replaced
# OUTPUT: Data frame of dimension x_m by 4, where each column is the deterministic utilites, 
#             column 1 is for keeping the engine from x=0 to x_m, engine type s_1
#             column 2 is for keeping the engine from x=0 to x_m, engine type s_2
#             column 3 is for replacing the engine from x=0 to x_m, engine type s_1
#             column 4 is for replacing the engine from x=0 to x_m, engine type s_2

values <- function(s=c(s_1,s_2),x = 0:x_m){
  u <- mdply(x,
              function(t){
                theta_1 * s - theta_2 * t
              }) %>%
    mutate(V3 = rep(0,length(x)),V4 = rep(0,length(x))) %>%
    select(-X1)
}


