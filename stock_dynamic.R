stock_dynamic <- function(r,K,X0, p,c,E,C){
  
  catch_by_spe <- colSums(q_matrix*E)
  last_stock <- X0
  stock_value <- X0
  for(i in 1:year){
    tmp_stock = last_stock+r*last_stock*(1-last_stock/K)-catch_by_spe*last_stock
    stock_value <- append(stock_value, tmp_stock)
    last_stock <- tmp_stock
  }
  stock_value <- matrix(stock_value, ncol=species_num, nrow=(year+1), byrow=TRUE)
  return(stock_value)
}
