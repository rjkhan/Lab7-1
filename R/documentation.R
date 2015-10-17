#' Ridge regression
#'
#' This is our own created ridge regression function
#'
#' This is a linear regression with one extra shrinkage parameter(lambda).
#' Note that the data should be normalized before using this function. 
#' @param formula A formula on the form response~first_predictor + second_predictor + ... . The response variable and all predictors must be in the data set provided.
#' @param data The data set from which to choose the predictors and the response variable in the formula.
#' @param lambda The value of lambda, lambda = 0 as default which is equals to OLS.
#' @examples
#' ridgeregr(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=10)
#' @return An object of S3-type class "ridgeregr": A list of nine elements, including fitted values and residuals.
#' @seealso
#' The methods that use the ridgeregr class:\link{predict.ridgeregr}.
#' @export
"ridgeregr"

#' Print the fitted values of a ridgeregr object
#'
#' A method that prints out the fitted values of the ridge regression model, possibly for new data.
#'
#' @param x A ridgeregr object.
#' @param x_values Optional. If included, the predictions are made on these x_values instead.
#' @examples
#' a <- ridgeregr(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=10)
#' predict(a) # The fitted values
#' df <- data.frame(x1=runif(n = 150, min = -1.5, max = 1.5), x2=runif(n = 150, min = -2, max = 2.5) )
#' predict(x=a, x_values = df) # Predicted on the new x-values in the df data.frame.
#' @return a numeric vector of  fitted values.
#' @export
#' @seealso \link{predict}. The generic function from which predict.ridgeregr is created.
"predict.ridgeregr"