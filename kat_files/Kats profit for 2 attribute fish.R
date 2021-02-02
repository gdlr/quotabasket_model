# write a function to calculate the profit for one technology using 2 diff fish characteristics
profit_tech <- function(E){
  stock <- stock_dynamic(K = parameter$K, X0 = parameter$X0, p = parameter$p, c = parameter$c, C = C, r = parameter$r, E = E)
  harvest <- NULL
  ratio <- rat_new
  catch_by_techxspecies_combo <-  q_matrix*E
  for(i in 1:tech_num){
    tmp <-  catch_by_techxspecies_combo[1,]
    dim(tmp) <- c(species_num,1)
    tmp_prop_catch_species_att <- tmp*ratio
    tmp_prop_rev_species_att <- tmp_prop_catch_species_att*p
    
      stock %*% tmp_prop_catch_species_att
    
    harvest <- append(harvest, tmp_harvest)
  }
  x = matrix(harvest,ncol=tech_num,nrow=(year+1))
  return(x)
}

