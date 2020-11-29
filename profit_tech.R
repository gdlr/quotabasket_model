# write a function to calculate the profit for one technology
profit_tech <- function(r,K,X0, p,c,E,C, q_matrix){
  stock <- stock_dynamic(r,K,X0, p,c,E,C)
  harvest <- NULL
  catch_by_techxspecies_combo <-  q_matrix*E
  for(i in 1:tech_num){
    tmp <-  catch_by_techxspecies_combo[i,]
    dim(tmp) <- c(species_num,1)
    tmp_harvest <- (stock %*% (tmp*p)) - C[i]*E[i]
    harvest <- append(harvest, tmp_harvest)
  }
  x = matrix(harvest,ncol=tech_num,nrow=(year+1))
  return(x)
}

