# Random draw of whether to replace the engine conditional on the CCPs at each mileage, for each brand.
# Draws all the observations needed for each brand (using type_Size) at once for computational efficiency.
# INPUT: type:      the brand type doing the random draw for
#        type_Size: the number of draws for that brand
# OUTPUT:           an x_m by type_Size array of 0-1s indicating whether an engine is replaced
#                   rows are for each mileage, and columns are for each draw
#                   note that the rows after the first replacement for each column is irrelevant
#                   as all data is later truncated at the first replacement using which.min

todeath <- function(type, type_Size){
  laply(1:x_m, function(l) rbinom(type_Size, 1, p[l, type]))
}
