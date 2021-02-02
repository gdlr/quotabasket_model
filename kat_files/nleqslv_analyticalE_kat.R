# analytical E
# this function finds the optimal E1, E2, E3 by also finding values for lambda1 and lambda2

library(nleqslv)
library(tidyverse)


optE_L <- function(p1,p2,p3,q11,q12,q13,q21,q22,q23,q31,q32,q33,x1,x2,x3,c1,c2,c3){
  y <- numeric(5)
  
  y[1] <- -(q22*E2*x2 + q23*E3*x2 + q12*E2*x1 + q13*E3*x1) / (q11*x1 - q21*x2)
  y[2] <- (p1*q11*x1 + p2*q21*x2 + p3*q31*x3 - E1*2*c1 - L2*q31*x3) / (q11*x1 + q21*x2)
  y[3] <- (p1*q12*x1 + p2*q22*x2 + p3*q32*x3 - L1*(q12*x1 + q22*x2) - E2*2*c2) / (q32*x3)
  y[4] <- (p1*q13*x1 + p2*q23*x2 + p3*q33*x3 - L1*(q13*x1 + q23*x2) - L2*q33*x3)/(2*c3)
  y[5] <- (-q31*E1*x3 - q32*E2*x3) / (q33*x3)

  return(c(E1,E2, E3, L1, L2))
}

# test

optE_L(10,20,30,.1,.2,.3,.4,.5,.6,.7,.8,.9,1000,2000,3000,5,10,15)

# practice

optE_L <- function(parameterz){
  p1 <- parameterz[1]
  p2 <- parameterz[2]
  p3 <- parameterz[3]
  q11 <- parameterz[4]
  q12 <- parameterz[5]
  q13 <- parameterz[6]
  q21 <- parameterz[7]
  q22 <- parameterz[8]
  q23 <- parameterz[9]
  q31 <- parameterz[10]
  q32 <- parameterz[11]
  q33 <- parameterz[12]
  x1 <- parameterz[13]
  x2 <- parameterz[14]
  x3 <- parameterz[15]
  c1 <- parameterz[16]
  c2 <- parameterz[17]
  c3 <- parameterz[18]
  
  y <- numeric(5)
  
  y[1] <- -(q22*y[2]*x2 + q23*y[3]*x2 + q12*y[2]*x1 + q13*y[3]*x1) / (q11*x1 - q21*x2)
  y[2] <- (p1*q11*x1 + p2*q21*x2 + p3*q31*x3 - y[1]*2*c1 - y[5]*q31*x3) / (q11*x1 + q21*x2)
  y[3] <- (p1*q12*x1 + p2*q22*x2 + p3*q32*x3 - y[4]*(q12*x1 + q22*x2) - y[2]*2*c2) / (q32*x3)
  y[4] <- (p1*q13*x1 + p2*q23*x2 + p3*q33*x3 - y[4]*(q13*x1 + q23*x2) - y[5]*q33*x3)/(2*c3)
  y[5] <- (-q31*y[1]*x3 - q32*y[2]*x3) / (q33*x3)
  
  return(y)
  
}

parameters <- c(10, 20, 30, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1000, 2000, 3000, 5, 10, 15)
ystart <- c(rep(0.1, 5))

test <- nleqslv(ystart, optE_L)
