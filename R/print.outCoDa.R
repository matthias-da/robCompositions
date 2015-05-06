#' print method for outCoDa objects
#' 
#' Gives a short information of the amount of outliers in objects of class
#' \sQuote{outCoDa}.
#' 
#' 
#' @param x object of class \sQuote{ourCoDa}
#' @param \dots ...
#' @author Matthias Templ, Karel Hron
#' @seealso \code{\link{outCoDa}}
#' @keywords print
#' @examples
#' 
#' data(expenditures)
#' oD <- outCoDa(expenditures)
#' oD
#' 
print.outCoDa <- function(x, ...){
 cat("\n --------------------\n")	
 print(paste(length(which(x$outlierIndex == TRUE)), "out of", length(x$outlierIndex), "observations are detected as outliers."))
 cat("\n --------------------\n\n")		
}
