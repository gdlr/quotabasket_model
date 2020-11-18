# write a function calculate the harvest of all technology
harvest_tech <- function(E){
  harvest_tech <- NULL
  
  for(i in 1:tech_num){
    harvest_tech <- cbind(harvest_tech, one_tech_harvest(i,E))
  }
  return(harvest_tech)
}