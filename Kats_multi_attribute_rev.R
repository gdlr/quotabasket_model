# calc revenue per species for fish w/ multiple attributes

p_new_test <- p_new[1:3000,]
ratio <- ratio_new
p <- p_new_test


rev_spec <- function(E, p, ratio){
  
  harvest <- as.data.frame(harvest_species(E))
  new_array <- matrix(nrow = nrow(harvest), ncol = 2*ncol(harvest))
  for (i in 1:ncol(harvest)){
    new_array[,2*i - 1] <- harvest[,i]
    new_array[,2*i] <-  harvest[,i]
  }
  
  for(i in 1:nrow(p)){
    for(j in 1:nrow(ratio)){
      ratio_rev <- p[i,]*ratio[j,]
    }
  }
  
  for(j in 1:nrow(ratio_rev)){
    for(i in 1:ncol(new_array))
    rev_spec_att <- new_array[,i]*ratio_rev[j,i]
  }
  

  for(i in 1:species_num){
    tmp <-  harvest[,i]
    dim(tmp) <- c(year+1,1)
    rev_spec_att1 <- tmp * ratio_rev[i,i]
    rev_spec_att2 <- tmp * ratio_rev[i,i+1]
    rev_spec <- rowSums(rev_spec_att)
  }
return(matrix((rev_spec),nrow=(year+1), ncol = species_num)
}
   
    
    # column from harvest * row in ratio = per yr... rev,spec,att1 | rev,spec,att2
    # 6x1 %*% 1x2 = 6x2
    
    # 2x1 * 1x2 = 2x1
    # column from prices * row in prop_attrib  = per yr... rev,spec,att1 // rev,spec,att2
    
    # prices = attrib x spec 2x4


# ratio * prices =

1296x8 * 8x70 = 1296x70

