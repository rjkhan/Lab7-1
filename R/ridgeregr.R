

ridgeregr <- function(formula, data, lambda = 0){
  
  X <- model.matrix(formula,data)
  X <- X[,-1,drop=FALSE]
  
  for(i in 1:ncol(X)){
    X[,i] <- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
  } 
  
  Xmodif <- sqrt(lambda) * diag(ncol(X))
  Xstar <- rbind(X,Xmodif)
  formulanames <- all.vars(formula)
  y <- data[,which(names(data) == formulanames[1])]
  y <- y - mean(y)
  ystar <- c(y, rep(0, ncol(X)))
  qrextar <- qr(Xstar)
  
  betavalues <- solve(qr.R(qrextar)) %*% t(qr.Q(qrextar)) %*% ystar
  
  predvalues <- X %*% betavalues
  
  dataname <- deparse(substitute(data))
  ridgereg_list <- list(formula = formula, data=data, dataname=dataname,
                        X=X, y=y, betavalues = betavalues, predvalues= predvalues)
  ridgereg_object <- structure(ridgereg_list, class="ridgeregr")
  
  return(ridgereg_object)
  
  
}