# write a function to calculate the profit for one technology
profit_tech <- function(E){
  stock <- stock_dynamic(E)
  harvest <- NULL
  catch_by_tech <-  q_matrix*E
  for(i in 1:tech_num){
    tmp <-  catch_by_tech[i,]
    dim(tmp) <- c(species_num,1)
    tmp_harvest <- (stock %*% (tmp*parameter$p)) - C[i]*E[i]
    harvest <- append(harvest, tmp_harvest)
  }
  x = matrix(harvest,ncol=tech_num,nrow=(year+1))
  return(x)
}

