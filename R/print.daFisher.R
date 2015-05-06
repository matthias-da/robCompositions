#' print method for objects of class daFisher
#' 
#' Provides a print output of objects from class \dQuote{daFisher} as shown in
#' the examples.
#' 
#' Have a look at the example, it's self-explaining.
#' 
#' @param x object of class \sQuote{daFisher}
#' @param \dots \code{\dots{}}
#' @return nothing
#' @author Matthias Templ
#' @seealso \code{\link{daFisher}}
#' @keywords print
#' @examples
#' 
#' require(MASS)
#' x1 <- mvrnorm(20,c(0,0,0),diag(3))
#' x2 <- mvrnorm(30,c(3,0,0),diag(3))
#' x3 <- mvrnorm(40,c(0,3,0),diag(3))
#' X <- rbind(x1,x2,x3)
#' grp=c(rep(1,20),rep(2,30),rep(3,40))
#' 
#' d1 <- daFisher(X,grp=grp,method="classical",coda=FALSE)
#' d2 <- daFisher(X,grp=grp,method="robust",coda=FALSE)
#' d2
#' 
print.daFisher <- function(x,...){
	cat("--------------------------------------")
	cat("\nResults from Fishers discriminant analysis, coda ==", x$coda)
	cat("\n- Variance between the classes: \n")
	print(x$B)
	cat("\n- Variance within the classes: \n")
	print(x$W)
	cat("\n- Loadings matrix: \n")
	print(x$load)
	cat("--------------------------------------\n")
}
