# Harvest to stock fxn

stocks <- function(E, B, Z, X_0){

  #      (sxs)(sxn)(nx1)
  Hvst <- B%*%Z%*%E
  
  # stock in period t+1
  Xt1 <- X_0 - Hvst
    
  return(Xt1)
}



