# Thu Jun 27 09:47:44 2019

# Author: Jeffrey Durieux, MSc

# What: function that computes all pairwise similarties (with the modified RV)
# input: a list object where each element is a matrix of one subject
# output: if argument dist == TRUE then a dissimilarity matrix else a similarity matrix

computeRVmat <- function(DataList = DataList, dist = TRUE, verbose = TRUE){
  
  if( exists('modRV') == FALSE){
    stop('The modifiedRV function is not in your Global Environment')
  }
  
  N <- length(DataList)
  
  comb <- t(combn(1:N, 2))
  
  if(verbose == TRUE){
    pb <- txtProgressBar(min = 0, max = nrow(comb), initial = 0)
    
    RVsS <- matrix(data = NA, nrow = N , ncol = N)
    RVS <- numeric()
    
    for(i in 1:nrow(comb)){
      RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])
      
      res <- c(comb[i , ] , RVS[i] )
      
      RVsS[res[1]  , res[2] ] <- res[3]
      setTxtProgressBar(pb, i)
    }
  }else{
    RVsS <- matrix(data = NA, nrow = N , ncol = N)
    RVS <- numeric()
    
    for(i in 1:nrow(comb)){
      RVS[i] <- modRV( DataList[[ comb[i,1] ]] , DataList[[ comb[i,2] ]])
      
      res <- c(comb[i , ] , RVS[i] )
      
      RVsS[res[1]  , res[2] ] <- res[3]
    }
  }
  
  RVsS[lower.tri(RVsS)] = t(RVsS)[lower.tri(RVsS)]
  diag(RVsS) <- 1
  
  if(dist == TRUE){
    RVsS <- as.dist(1-RVsS)
  }
  return(RVsS)
}

# x1 <- matrix(rnorm(1000),100)
# x2 <- matrix(rnorm(1000),100)
# x3 <- addError(x2,error = 0.5)
# X <- list(x1,x1,x1,x2,x2,x2,x3,x3)
# 
# test <- computeRVmat(DataList = X, dist = T, verbose = T)
# test <- computeRVmat(DataList = X, dist = F, verbose = F)
# 
# test <- computeRVmat(DataList = X, dist = T, verbose = F)
# test <- computeRVmat(DataList = X, dist = F, verbose = T)
