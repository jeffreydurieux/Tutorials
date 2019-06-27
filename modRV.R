# Thu Jun 27 09:30:46 2019 ------------------------------

# Author: Jeffrey Durieux, MSc

# What: R-function of the modified RV-coefficient

# reference: Smilde AK, Kiers HAL, Bijlsma S, Rubingh CM, van Erk MJ (2009) Matrix correlations for high-dimen- sional data: the modified RV-coefficient. Bioinformatics 25(3):401–405


modRV <- function(X, Y){
  
  XXtilde <- ( X %*% t(X) ) - diag (diag( X %*% t(X) ) )
  YYtilde <- ( Y %*% t(Y) ) - diag (diag( Y %*% t(Y) ) )
  
  res <-  ( t(c(XXtilde)) %*% c(YYtilde) ) / 
    ( sqrt( ( t(c(XXtilde)) %*% c(XXtilde)) * ( t(c(YYtilde)) %*% c(YYtilde)) ) ) 
  
  
  return(res)
}


X1  <- matrix(rnorm(100*300),100,300)
usv <- svd(X1)
X2  <- usv$u[,-3] %*% diag(usv$d[-3]) %*% t(usv$v[,-3])


RV2(X1,X2)
modRV(X1,X2)
