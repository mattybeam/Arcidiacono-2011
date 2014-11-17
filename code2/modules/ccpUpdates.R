#####Notes
#ccpUpdates.R
#I move these functions into a seperate module, since I use them both for simulating the data and estimating the model
#I used cumprod() to convert the CCPs into the overall likelihood of replacing in a given period. I then draw my data with 
#sample_n(), weighting by these likelihoods.
#precision is much higher around 0 than around 1, so calculate and store CCP and 1-CCP seperatly
#lead(CCP, default=last(CCP), order_by=x.t): set default to avoid NA, and set order_by to sort properly
##########

gamma.Operator <- function(CCP, theta, beta){
  #Inputs: dataframe of CCPs, CCP, and a vector of cost parameters, theta
  #Output: dataframe of CCPs after one value function iteration
  group_by(CCP, s) %>%
    mutate(
      CCP.replace = first(prob.replace),
      CCP.dont.replace = lead(prob.replace, default=last(prob.replace), order_by=x.t),
      f = theta[1]*s-theta[2]*x.t+beta*(log(CCP.replace)-log(CCP.dont.replace)),
      #for precision, calculate and store CCP and 1-CCP seperatly 
      prob.replace = 1/(1+exp(f)),
      prob.dont.replace = exp(f)/(1+exp(f))
    ) %>%
    select(-c(CCP.replace, CCP.dont.replace, f)) %>%
    ungroup
}

CCP.to.likelihood <- function(CCP){
  #Input: dataframe of CCPs, CCP, and vector of cost parameters, theta
  #Output: dataframe of likelihoods, which indicate the probability of replacing after exactly t periods
  group_by(CCP, s) %>%
    mutate(likelihood = prob.replace*cumprod(lag(prob.dont.replace, default=1, order_by=x.t))) %>%
    select(-c(prob.replace, prob.dont.replace)) %>%
    ungroup
}
