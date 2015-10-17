predict.ridgeregr <- function(x, x_values = "default"){ 

  
  if(identical(x_values, "default")){ 
  
    return(x$predvalues) 
  }else{ 

  X <- as.matrix(x_values)
  
  for(i in 1:ncol(X)){
    X[,i] <- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
  } 
  
  
  y_hat <- X %*% x$betavalues 
      return(y_hat) 
  }

}
