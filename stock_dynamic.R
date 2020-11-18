# write a function to calculate the stock dynamic
stock_dynamic <- function(species_index, E){
  
  stock= NULL
  stock=append(stock, parameter$X0[species_index])
  
  for( i in 2:year){
    
    # apply the first equation in the "Our Model" section
    X = stock[i-1]+parameter$r[species_index]*stock[i-1]*(1-stock[i-1]/parameter$K[species_index]) - sum(species_tech_matrix[species_index]*E)*stock[i-1]
    
    stock=append(stock, X)
    
  }
  return(stock)
}