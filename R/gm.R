#' @title
#' gmean
#' @description
#' This function calculates the geometric mean.
#' 
#' @param x a vector 
#' 
#' @details
#' \code{gm} calculates the geometric mean for all positive entries of a vector. 
#' Please note that there is a faster version available implemented with Rcpp
#' but it currently do not pass CRAN checks cause of use of Rcpp11 features. This C++ version
#' accounts for over- and underflows. It is placed in inst/doc
#' @export
#' @author Matthias Templ
#' @examples
#' gm(c(3,5,3,6,7))
gm <- function (x) {
	if(!is.numeric(x)) stop("x has to be a vector of class numeric")
	if (any(na.omit(x == 0)))
		0
	else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
}
