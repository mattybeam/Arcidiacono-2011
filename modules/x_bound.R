x_bound<- function(eta, s){
  ceiling(theta_1*s - log(eta / (1- eta))) / theta_2
}