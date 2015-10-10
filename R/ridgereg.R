ridgereg <- setRefClass("ridgereg",
                      
                      fields = list( formula = "formula",
                                     data = "data.frame",
                                     dataname = "character",
                                     lambda = "numeric",
                                     coefficients = "matrix",
#                                      residual = "matrix",
                                     predicted = "matrix"),
#                                      df = "integer",
#                                      residualvar = "numeric",
#                                      varregcoefficients = "matrix",
#                                      tvalues = "matrix",
#                                      pvalues = "matrix"),
                      
                      methods = list(
                        initialize = function(formula, data, lambda ){
                          "From inputs formula and data, generates all the other fields upon object initialization."
                          .self$formula <- formula
                          .self$data <- data
                          .self$lambda <- lambda
                          .self$dataname <- deparse(substitute(data))
                          X <- model.matrix(formula,data)
                          
                          # We decide to use the sample variance var() built into R for V(X)
                          
                          for(i in 2:ncol(X)){
                            X[,i] <- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
                          }
                          
                          Xmodif <- sqrt(.self$lambda) * diag(ncol(X))
                          Xstar <- rbind(X,Xmodif)
                          
                          formulanames <- all.vars(formula)
                          y <- data[,which(names(data) == formulanames[1])]
                          ystar <- c(y, rep(0, ncol(X)))
                          qrextar <- qr(Xstar)
                          
                          
                          
                          
                          
                          .self$coefficients <- solve(qr.R(qrextar)) %*% t(qr.Q(qrextar)) %*% ystar
                          .self$predicted <- X %*% .self$coefficients
#                          .self$residual <- y -  .self$predicted
#                           .self$df <- dim(data)[1] - dim(X)[2]
#                           .self$residualvar <- sum(.self$residual * .self$residual) / .self$df
#                           .self$varregcoefficients <-.self$residualvar * solve(t(X) %*% X)
#                           .self$tvalues <- .self$coefficients / sqrt(diag(.self$varregcoefficients))
#                           .self$pvalues <- 2 * (1 - pt(abs(.self$tvalues),.self$df))    
                        },
                        print = function(){
                          "Gives a printout of the call as well as the calculated regression coefficients."
                          blender <- as.character(.self$formula)
                          formulastring <- paste(blender[2],blender[1],blender[3])
                          readout <- as.vector(.self$coefficients)
                          coefnames <- rownames(.self$coefficients)
                          names(readout) <- coefnames
                          writeLines(c("Call:"))
                          calline <- paste("ridgereg","(","formula = ",formulastring,", data= ",
                                           .self$dataname, ", lambda= ", .self$lambda, ")",sep="")
                          writeLines(c(calline,"","Coefficients:"))
                          return( readout )
                        },
                        coef = function(){
                          "Returns a named vector of the regression coefficients in the linear model." 
                          readout <- as.vector(.self$coefficients)
                          coefnames <- rownames(.self$coefficients)
                          names(readout) <- coefnames
                        return(readout)
                        },
                        predict = function(newframe=NULL){
                          "Returns the predicted values of the linear model."
                          if(length(newframe) != 0){
                            return(cbind(rep(1,length(newframe)),as.matrix(newframe)) %*% .self$coefficients)
                          }
                          return(.self$predicted[,1,drop=FALSE])
                        }
                        
                      )
)
      
