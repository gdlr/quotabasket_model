# write a function to calculate the harvest of one technology
one_tech_harvest <- function(tech_index, E){
  h_spe_tech = NULL
  # calculate the harvest for 1 species from 1 tech over time period, then sum the harvest for all species of 1 tech
  for(i in 1:species_num){
    x = species_tech_matrix[tech_index,i]*E[tech_index]*simul_stock_dynamic[i]
    h_spe_tech = c(h_spe_tech, x)
  }
  
  return(rowSums(data.frame(h_spe_tech)))
}