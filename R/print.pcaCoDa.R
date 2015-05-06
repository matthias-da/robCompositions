#' Print method for pcaCoDa objects
#' 
#' Print method for objects of class \sQuote{pcaCoDa}.
#' 
#' 
#' @param x object of class \sQuote{pcaCoDa}
#' @param \dots ...
#' @return Prints the (cummulative) percentages of explained variability for
#' clr transformed data by principal component analysis.
#' @author M. Templ, K. Hron
#' @seealso \code{\link{pcaCoDa}}, \code{\link{plot.pcaCoDa}}
#' @keywords print
#' @examples
#' 
#' data(expenditures)
#' p1 <- pcaCoDa(expenditures)
#' p1
#' plot(p1)
#' 
print.pcaCoDa <- function(x, ...){
	## percentage of explained variability for clr transformed data
	eV <- x$eigenvalues / sum(x$eigenvalues)
	eVcum <- cumsum(x$eigenvalues) / sum(x$eigenvalues)
	cat("\n-------------------")
	cat("\n Percentages of explained variability for compositional data after clr transformation \n")
	print(eVcum)
	cat("\n-------------------\n\n")	
}
