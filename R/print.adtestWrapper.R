#' print method for objects of class adtestWrapper
#' 
#' Provides a short print output as shown in the examples.
#' 
#' Have a look at the example, it's self-explaining.
#' 
#' @param x object of class \sQuote{adtestWrapper}
#' @param \dots \code{\dots{}}
#' @return nothing
#' @author Matthias Templ and Karel Hron
#' @seealso \code{\link{adtestWrapper}}, \code{\link{summary.adtestWrapper}}
#' @keywords print
#' @examples
#' 
#' data(machineOperators)
#' a <- adtestWrapper(machineOperators, R=50) # choose higher value of R
#' a
#' summary(a)
#' 
print.adtestWrapper <- function(x,...){
	if(all(x$check)){
		print(paste("The data follow the normal distribution on the simplex (alpha =",x$alpha,")",sep=""))
	} else { 
		print(paste("The data do not follow the normal distribution on the simplex (alpha =",x$alpha,")",sep=""))
		#print(x$check)
}
}
