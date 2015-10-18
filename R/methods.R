predict.ridgeregr <- function(x, x_values = "default"){ 

  
  if(identical(x_values, "default")){ 
  
    return(x$predvalues) 
  }else{ 
    
  x_values <- as.data.frame(x_values)
  x_values <- x_values[,-which(names(x_values) == x$respname)]
  X <- as.matrix(x_values)
  
  for(i in 1:ncol(X)){
    X[,i] <- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
  } 
  
  
  y_hat <- X %*% x$betavalues 
      return(y_hat) 
  }

}
