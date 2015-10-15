#'Lab7 package contains a RC generator for ridge regression analysis and some
#'data munging
#'
#'The Lab7 package consists of a single RC generator and some data munging. 
#'Package imports packages \code{ggplot2}, \code{nycflights13}, \code{mlbench}, 
#'\code{dplyr}, \code{caret}, \code{grid}, \code{gridExtra}.
#'
#'@name Lab7
#'@docType package
#'  
#'@section Lab7 functions: The Lab7 function \code{ridgereg} creates an 
#'  \code{ridgereg} class object (actually an environment in R) which does ridge
#'  regression analysis upon input formula and data.frame and parameter lambda.
#'  All the calculations are done and stored in the fields of the object upon
#'  initialization. The methods just returns the values.
#'  
NULL