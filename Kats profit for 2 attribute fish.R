# write a function to calculate the profit for one technology using 2 diff fish characteristics
profit_tech <- function(E){
  stock <- stock_dynamic(E)
  harvest <- NULL
  catch_by_techxspecies_combo <-  q_matrix*E
  for(i in 1:tech_num){
    tmp <-  catch_by_techxspecies_combo[i,]
    dim(tmp) <- c(species_num,1)
    tmp_prop_catch_species_att <- tmp*ratio
    tmp_prop_rev_species_att <- tmp_prop_catch_species_att*p_new
      stock %*% tmp_prop_catch_species_att
    
    harvest <- append(harvest, tmp_harvest)
  }
  x = matrix(harvest,ncol=tech_num,nrow=(year+1))
  return(x)
}

