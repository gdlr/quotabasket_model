mort_test <- function(species, tech, cost, years, mort_min, mort_max, steplength, baskets){
  m = seq(mort_min,mort_max,steplength)
  all_m_combn = expand.grid(replicate(length(baskets),m, F))
  mort_vec <- NULL
  for(i in 1:nrow(all_m_combn)){
    tmp_m <- t(all_m_combn[i,])
    tmp_output <- qb_stock(species, tech, cost, baskets, mortality = tmp_m, years)
    tmp_rev <- colSums(tmp_output$profit_per_t[-1,])
    mort_vec <- rbind(mort_vec, tmp_rev)
  }
  
  mort_vec = data.frame(mort_vec)
  
  cev = cbind(all_m_combn, rowSums(mort_vec)) %>% 
    arrange(-rowSums(mort_vec)) 
  
  return(cev[5,])
}
