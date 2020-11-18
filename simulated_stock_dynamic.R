# use the stock_dynamic function to simulate the stock dynamics for species
simulated_stock_dynamic <- function(E){
  simulated_stock_dynamic = NULL
  
  for(i in 1:species_num){
    stock = stock_dynamic(species_index = i,E)
    
    simulated_stock_dynamic = cbind(simulated_stock_dynamic, stock)
    
  }
  return(simulated_stock_dynamic)
}

