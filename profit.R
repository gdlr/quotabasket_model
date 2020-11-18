# write a function to apply the function above to calculate the profit for all the technologies

profit <- function(E){
  profit = NULL
  
  for(i in 1:tech_num){
    profit = cbind(profit, profit_tech(i,E))
  }
  return(profit)
}
