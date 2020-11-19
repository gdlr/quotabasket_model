harvest_species <- function(E){
  harvest <- NULL
  catch_by_spe <- colSums(q_matrix*E)
  stock <- stock_dynamic(E)
  for(i in 1:species_num){
    harvest <- append(harvest, stock[,i]*catch_by_spe[i])
  }
  return(matrix(harvest,nrow=(year+1), ncol = species_num))
}

