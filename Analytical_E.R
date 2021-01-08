#### Analytical E

## linear costs

Opt_E1 <- function(Q, c, q1, q2, p1, p2, X1, X2){
  Eopt <- (c*Q + p1*q2*Q*X2 - p2*q2*Q*X2) / (p1*q1*q2*X1*X2 - p2*q1*q2*X1*X2)
  
  return(Eopt)
  }

Opt_E2 <- function(Q, c, q1, q2, p1, p2, X1, X2){
  Eopt <- (c*Q + p2*q1*Q*X1 - p1*q1*Q*X1) / (p2*q2*q1*X2*X1 - p1*q2*q1*X1*X2)
  
  return(Eopt)
}

#test

Opt_E1(Q = 500, c = 10, q1 = 0.5, q2 = 0.5, p1 = 90, p2 = 100, X1 = 1000, X2 = 1000)

## increasing costs

