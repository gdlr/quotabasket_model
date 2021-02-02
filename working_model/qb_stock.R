qb_stock <- function(species, tech, cost, baskets, mortality, years){
  
  # Make useful data frames of each of our lists: this allows any number to be added.
  sp_df <- as.data.frame(do.call(rbind, species))
  tech_df <- as.data.frame(do.call(rbind, tech))
  cost_df <- as.data.frame(do.call(rbind, cost))
  baskets_df <- as.data.frame(do.call(rbind, baskets))
  
  # Calculate number of tech and species
  ntech <- nrow(tech_df)
  nspecies <- nrow(sp_df)
  
  # Add starting stock values.
  stock_df <- data.frame(t(sp_df$X))
  
  # Make a few full data frames we can fill later
  h_df <- data.frame(matrix(ncol = nspecies))
  e_df <- data.frame(matrix(ncol = ntech))
  
  ## Profits:
  pft_df <- data.frame(matrix(ncol = ntech))
  pft_sp_df <- data.frame(matrix(ncol = nspecies))
  
  #WWWW
  # We still probably want to calculate harvest per species AND technology...
  #WWWW
  
  
  ### --------
  ### Unchanging Parameters
  ### --------
  
  # D is the cost matrix, where the diagonal values are the cost of each tech:
  D <- matrix(0, nrow = ntech, ncol = ntech)
  diag(D) <- 2*cost_df$cost
  
  # Z is the the tech matrix
  Z <- t(as.matrix(tech_df[,-1]))
  
  # P is the price matrix
  P <- as.matrix(sp_df$p)
  
  # M is whether a species is contained in a basket:
  M <- as.matrix(baskets_df)
  
  # We build a diagonal matrix (nxn) for the second constraint
  N <- matrix(0, nrow=ntech,ncol=ntech)
  diag(N) <- 1
  
  ### --------
  ### Changing Parameters
  ### --------
  
  for (i in 1:years){
    ### ------
    ### Calculate effort per year
    ### ------
    E <- NULL
    # B, where the diagonal corresponds to stock sizes (X) for each species
    B <- matrix(0, nrow = nspecies, ncol = nspecies)
    diag(B) <- as.numeric(tail(stock_df,1)) ### Stock goes here...
    # d combines the P,B,and Z matrices yielding the per tech revenue (1xtech)
    # (1xspecies)x(speciesxspecies)x(speciesxtech)
    d <- t(t(P)%*%B%*%Z)
    # A is the transpose matrix that defines which basket each species is in:
    A <- -1*M%*%B%*%Z
    # Add the second constraint N:
    A <- rbind(A, N)
    # Take the transpose for solve.QP:
    t_A <- t(A)
    # We need to calculate the basket caps based on the initial BASKET size(not stock size!):
    viz_stock <- rowSums(tail(stock_df,1)) ### And here...
    # Make the first constraint based on mortality:
    b <- -M%*%(mortality*t(as.matrix(tail(stock_df, 1))))
    # Make a second constraint so they're all greater than zero:
    b2 <- matrix(0, nrow=ntech, ncol=1)
    # Bind them together
    b <- rbind(b, b2)
    # Find optimal efforts
    E <- solve.QP(D,d,t_A,b, meq=0)$solution # Vector where the position is the effort per tech.
    
    ### ------
    ### Calculate harvest for each species for the year
    ### ------
    # Repeat E for each species, so we have effort per tech per species:
    h <- c(B%*%Z%*%E) # Position corresponds to the species:
    # Calculate stock growth with X(t+1) = Xt + Xt*r*(1-Xt/K) - h
    stock_next <- tail(stock_df,1) + sp_df$r*tail(stock_df,1)*(1-tail(stock_df,1)/sp_df$K) - h
    stock_df <- rbind(stock_df, stock_next)
    
    ### Calculate profit for each technology for the year
    ### -----
    
    # (1xtech)*(techx1)
    Rev <- as.data.frame(t(d)*E)
    
    # .5 * (1xtech) * (techxtech) x (techx1)
    Costz <- as.data.frame((1/2)*t(E)%*%D*E)
    
    pft <- as.data.frame(Rev - Costz)
    names(pft) <- names(pft_df)
    
    ### Profit per species
    ### -----
    
    # (1xtech)*(techxspecies) = (1xspecies)
    # profit per tech x catchability
    
    # pft_sp <- pft*Z
    
    h_df <- rbind(h_df, h)
    e_df <- rbind(e_df, E)
    pft_df <- rbind(pft_df, pft)
    # pft_sp_df <- rbind(pft_sp_df, pft_sp)
    
  }
  
  rownames(stock_df) <- NULL
  colnames(stock_df) <- sp_df$s
  return(list(stock = stock_df, harvest = h_df, effort = e_df, profit = pft_df))
  
  # profit_species = pft_sp_df))
}