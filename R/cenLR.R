#' Centred logratio coefficients
#' 
#' The centred logratio (clr) coefficients map D-part compositional data from the simplex
#' into a D-dimensional real space.
#' 
#' Each composition is divided by the geometric mean of its parts before the
#' logarithm is taken.
#' 
#' @param x multivariate data, ideally of class data.frame or matrix
#' @param base a positive or complex number: 
#' the base with respect to which logarithms are computed. Defaults to \code{exp(1)}.
#' @return the resulting clr coefficients, including \item{x.clr}{clr coefficients}
#' \item{gm}{the geometric means of the original compositional data.}
#' @note The resulting data set is singular by definition.
#' @author Matthias Templ
#' @seealso \code{\link{cenLRinv}}, \code{\link{addLR}}, \code{\link{pivotCoord}},
#' \code{\link{addLRinv}}, \code{\link{pivotCoordInv}}
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman and Hall Ltd., London (UK). 416p.
#' @keywords manip
#' @export
#' @examples
#' 
#' data(expenditures)
#' eclr <- cenLR(expenditures)
#' inveclr <- cenLRinv(eclr)
#' head(expenditures)
#' head(inveclr)
#' head(pivotCoordInv(eclr$x.clr))
#' 
cenLR <- function(x, base = exp(1)){
	#if(dim(x)[2] < 2) stop("data must be of dimension greater equal 2")
	if(dim(x)[2] == 1){
		res <- list(x.clr=x, gm=rep(1,dim(x)[1]))	    	
	} else{
		geometricmean <- function (x) {
			if (any(na.omit(x == 0)))
				0
			else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
		}
		gm <- apply(x, 1, geometricmean)
		x.clr <- log(x/gm, base)
		res <- list(x.clr=x.clr, 
				gm=gm
		)
	}
	class(res) <- "clr"
	return(res)  
}
