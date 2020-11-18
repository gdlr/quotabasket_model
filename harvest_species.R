# write a function to calculate the harvest for  each of the species
harvest_species <- function(E){
  
  harvest_species = NULL
  for(i in 1:species_num){
    # apply the 2.1 equation in the "Our Model" section
    X = sum(species_tech_matrix[i]*E)*simul_stock_dynamic[i]
    
    harvest_species=c(harvest_species, X)
    
  }
  return(harvest_species)
}