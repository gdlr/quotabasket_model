# Analytical E

Optim_E <- function(r, k, q, p, X0, Xt, B, c){
  log_E <- (1/B)*(log10(-B*( -p*q*X0*r - c*r + (2*r*p*q*(X0^2)/k) + (2*r*X0*c/k) ) / ( B*( (q^2)*p*X0 + q*c ) - p*(q^2)*X0 ) ) )
  
  Eopt <- 10^(log_E)
  
  return(Eopt)
  }

# test <- Optim_E(.2,1,.5,200,200,300,.2,100)
# print(test)
# 104,330,389.74457
# it works! (I think... I mean it produces a number)