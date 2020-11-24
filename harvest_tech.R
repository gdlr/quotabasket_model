# write a function calculate the harvest of all technology
harvest_tech <- function(E){
  stock <- stock_dynamic(E)
  harvest <- NULL
  catch_by_tech <-  q_matrix*E
  for(i in 1:tech_num){
    tmp <-  catch_by_tech[i,]
    dim(tmp) <- c(species_num,1)
    tmp_harvest <- stock %*% tmp
    harvest <- append(harvest, tmp_harvest)
    }
  x = matrix((harvest),nrow=(year+1), ncol = tech_num)
  return(x)
}


