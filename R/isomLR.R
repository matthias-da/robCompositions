#' Isometric log-ratio transformation
#' 
#' An isometric log-ratio transformation with a special choice of the balances
#' according to Hron et al. (2010).
#' 
#' The isomLR transformation moves D-part compositional data from the simplex
#' into a (D-1)-dimensional real space isometrically.  From this choice of the
#' balances, all the relative information of the part \eqn{x_1} from the
#' remaining parts is separated. It is useful for estimating missing values in
#' \eqn{x_1} by regression of the remaining variables.
#' 
#' @param x object of class data.frame or matrix with positive entries
#' @param fast if TRUE, approx. 10 times faster but with numerical problems for
#' high-dimensional data.
#' @return The isomLR transformed data.
#' @author Karel Hron, Matthias Templ
#' @seealso \code{\link{isomLRinv}}
#' @references Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and C.
#' Barcel'o-Vidal (2003) Isometric logratio transformations for compositional
#' data analysis. \emph{Mathematical Geology}, \bold{35}(3) 279-300. \
#' 
#' Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values
#' for compositional data using classical and robust methods
#' \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages
#' 3095-3107.
#' @keywords math
#' @examples
#' 
#' require(MASS)
#' Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)
#' z <- isomLRinv(mvrnorm(100, mu=c(0,2), Sigma=Sigma))
#' 
#' data(expenditures)
#' isomLR(expenditures)
#' 
#' x <- exp(mvrnorm(2000, mu=rep(1,10), diag(10)))
#' system.time(isomLR(x))
#' system.time(isomLR(x, fast=TRUE))
#' 
#' 
"isomLR" <- function(x, fast=FALSE){
	x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
	D=ncol(x)
        if(fast){
	  for (i in 1:ncol(x.ilr)){
	     x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
	  }
	} else {
  	  for (i in 1:ncol(x.ilr)){
#		x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
		x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(apply(as.matrix(x[,(i+1):D]), 1, gm)/(x[,i]))	
	  } 
	}
	return(x.ilr)
}
