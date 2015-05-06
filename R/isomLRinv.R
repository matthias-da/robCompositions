#' Inverse isometric log-ratio transformation
#' 
#' The inverse transformation of \sQuote{isomLR()}.
#' 
#' For details on the choice of the balances, please, see at the research
#' report for which the link is given below.
#' 
#' @param x.ilr data frame or matrix
#' @return The transformed data.
#' @author Karel Hron
#' @seealso \code{\link{isomLR}}
#' @references Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and C.
#' Barcel'o-Vidal (2003) Isometric logratio transformations for compositional
#' data analysis. \emph{Mathematical Geology}, \bold{35}(3) 279-300 \
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
#' set.seed(123)
#' z <- mvrnorm(100, mu=c(0,2), Sigma=Sigma)
#' x <- isomLRinv(z)
#' head(x)
#' 
"isomLRinv" <- function(x.ilr){
	
	y=matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
	D=ncol(x.ilr)+1
	y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
	for (i in 2:ncol(y)){
		for (j in 1:(i-1)){
			y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
		}
	}
	for (i in 2:(ncol(y)-1)){
		y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
	}
	yexp=exp(y)
	x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
	return(x.back)
	#return(yexp)
}
