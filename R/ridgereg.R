#' A reference class generator for ridge regression
#' 
#' This function creates a class \code{ridgereg} object which does ridge 
#' regression upon a specified formula and data.frame and parameter lambda.
#' Ridge regression is performed by QR decompsition. Plots are handled by 
#' packages \code{ggplot2}, \code{grid} and \code{gridExtra}.
#' 
#' This RC class generator can create a \code{ridgereg} object from any formula
#' as long as the response are not factors. The QR decomposition calculations 
#' are all done using matrices. The printouts are simple \code{paste} and 
#' \code{writeLines} calls for the most part, and are not objects. Every method 
#' returns something relevant.
#' 
#' @field formula A formula taken as input, upon which linear regression is 
#'   performed.
#'   
#' @field data A data.frame which has names to which the names in the formula 
#'   refer.
#'   
#' @field dataname A character string containing the name of the data.
#'   
#' @field lambda A numeric scalar which specifies the hyperparameter lambda
#'   
#' @field coefficients A single column matrix consisting of the calculated 
#'   regression coefficients of the linear model.
#'   
#'   
#' @field predicted A single column matrix containing the linearly predicted 
#'   response.
#'   
#'   
#' @references \url{http://en.wikipedia.org/wiki/Tikhonov_regularization}
#'   
#' @examples
#' data(faithful)
#' ridgeobject <- ridgereg(formula = eruptions ~ waiting, data = faithful,lambda = 5)
#' ridgeobject$print()

ridgereg <- setRefClass("ridgereg",
                        
    fields = list( formula = "formula",
        data = "data.frame",
        dataname = "character",
        lambda = "numeric",
        coefficients = "matrix",
#       residual = "matrix",
        predicted = "matrix"),
#       df = "integer",
#       residualvar = "numeric",
#       varregcoefficients = "matrix",
#       tvalues = "matrix",
#       pvalues = "matrix"),
                      
    methods = list(
        initialize = function(formula, data, lambda ){
"From inputs formula and data, generates all the other fields upon object initialization."
        .self$formula <- formula
        .self$data <- data
        .self$lambda <- lambda
        .self$dataname <- deparse(substitute(data))
        X <- model.matrix(formula,data)
        X <- X[,-1,drop=FALSE]
                          
                          # We decide to use the sample variance var() built into R for V(X)
        for(i in 1:ncol(X)){
            X[,i] <- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
        } 
        Xmodif <- sqrt(.self$lambda) * diag(ncol(X))
        Xstar <- rbind(X,Xmodif)
        formulanames <- all.vars(formula)
        y <- data[,which(names(data) == formulanames[1])]
        y <- y - mean(y)
        ystar <- c(y, rep(0, ncol(X)))
        qrextar <- qr(Xstar)
        .self$coefficients <- solve(qr.R(qrextar)) %*% t(qr.Q(qrextar)) %*% ystar
        .self$predicted <- X %*% .self$coefficients
#       .self$residual <- y -  .self$predicted
#       .self$df <- dim(data)[1] - dim(X)[2]
#       .self$residualvar <- sum(.self$residual * .self$residual) / .self$df
#       .self$varregcoefficients <-.self$residualvar * solve(t(X) %*% X)
#       .self$tvalues <- .self$coefficients / sqrt(diag(.self$varregcoefficients))
#       .self$pvalues <- 2 * (1 - pt(abs(.self$tvalues),.self$df))    
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
     "Returns the predicted values of the linear model. Can also take a new data.frame
     with covariate values and make new prediction based on them."
        if(length(newframe) != 0){
          return(as.matrix(newframe) %*% .self$coefficients)
        }
          return(.self$predicted[,1,drop=FALSE])
        }
                        
    )
)
      
