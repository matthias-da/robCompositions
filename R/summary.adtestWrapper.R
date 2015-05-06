#' summary method for objects of class adtestWrapper
#' 
#' Provides a summary as shown in the examples.
#' 
#' A similar output is proposed by (Pawlowsky-Glahn, et al. (2008). In addition
#' to that, p-values are provided.
#' 
#' @param object object of class \sQuote{adtestWrapper}
#' @param \dots additional arguments passed through
#' @return a data frame including an information about the ilr-variables used
#' (first column), the underlying test (second column), the test statistics
#' (third column), the corresponding estimated p-values (fourth column) and an
#' information about the rejection of the null hypothesis (last column).
#' @author Matthias Templ and Karel Hron
#' @seealso \code{\link{adtestWrapper}}
#' @references Pawlowsky-Glahn, V., Egozcue, J.J. and Tolosana-Delgado, R.
#' (2008), \emph{Lecture Notes on Compositional Data Analysis} Universitat de
#' Girona, \url{http://dugi-doc.udg.edu/bitstream/10256/297/1/CoDa-book.pdf}
#' @keywords print
#' @examples
#' 
#' data(machineOperators)
#' a <- adtestWrapper(machineOperators, R=50) # choose higher value of R
#' a
#' summary(a)
#' 
summary.adtestWrapper=function(object,...){
	d=data.frame(ilrVars=unlist(object$info),
			testName=unlist(lapply(object$res,function(x) x$method)),
			testStat=unlist(lapply(object$res,function(x) x$statistic)),
			pvalue=unlist(lapply(object$res,function(x) x$p.value)),
			#alpha=object$alpha,
			check=object$check)
#d=lapply(res,function(x) x$info)
	string <- paste("Anderson-Darling test results ( alpha =", object$alpha, "):")
	string2 <- paste("--> p-values and tests are obtained from", object$est, "estimates.")
	cat("\n  -----------------------------------------------")	
	cat("\n ", string)
	cat("\n  ----------------\n")
	print(d)
	cat("\n  -----------------------------------------------\n")
	cat("\n ", string2)
	cat("\n")
	invisible(d)
}
