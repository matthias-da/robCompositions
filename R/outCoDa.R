#' Outlier detection for compositional data
#' 
#' Outlier detection for compositional data using standard and robust
#' statistical methods.
#' 
#' The outlier detection procedure is based on (robust) Mahalanobis distances
#' after a isometric logratio transformation of the data.  Observations with
#' squared Mahalanobis distance greater equal a certain quantile of the
#' Chi-squared distribution are marked as outliers.
#' 
#' If method \dQuote{robust} is chosen, the outlier detection is based on the
#' homogeneous majority of the compositional data set.  If method
#' \dQuote{standard} is used, standard measures of location and scatter are
#' applied during the outlier detection procedure.
#' 
#' @param x compositional data
#' @param quantile quantile, corresponding to a significance level, is used as
#' a cut-off value for outlier identification: observations with larger
#' (squared) robust Mahalanobis distance are considered as potential outliers.
#' @param method either \dQuote{robust} (default) or \dQuote{standard}
#' @param h the size of the subsets for the robust covariance estimation
#' according the MCD-estimator for which the determinant is minimized (the
#' default is (n+p+1)/2).
#' @return \item{mahalDist }{resulting Mahalanobis distance} \item{limit
#' }{quantile of the Chi-squared distribution} \item{outlierIndex }{logical
#' vector indicating outliers and non-outliers} \item{method }{method used}
#' @note It is highly recommended to use the robust version of the procedure.
#' @author Matthias Templ, Karel Hron
#' @seealso \code{\link{isomLR}}
#' @references Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and C.
#' Barcel'o-Vidal (2003) Isometric logratio transformations for compositional
#' data analysis. \emph{Mathematical Geology}, \bold{35}(3) 279-300. \
#' 
#' Filzmoser, P., and Hron, K. (2008) Outlier detection for compositional data
#' using robust methods. \emph{Math. Geosciences}, \bold{40} 233-248.\
#' 
#' Rousseeuw, P.J., Van Driessen, K. (1999) A fast algorithm for the minimum
#' covariance determinant estimator.  \emph{Technometrics}, \bold{41} 212-223.
#' @keywords multivariate
#' @examples
#' 
#' data(expenditures)
#' oD <- outCoDa(expenditures)
#' oD
#' 
outCoDa <- function(x, quantile=0.975, method="robust", h=1/2){
	if(dim(x)[2] < 2) stop("need data with at least 2 variables")
	
	covEst <- function(x, type) {
		standard <- function(x){
				list(mean=colMeans(x, na.rm=TRUE), varmat=cov(x))  
		}
		robust <- function(x){
				v <- covMcd(x)
				list(mean=v$center, varmat=v$cov)
		}
		switch(type,
				standard = standard(x),
				robust = robust(x))
	}
		
	z <- isomLR(x)
	cv <- covEst(z, method)
	dM <- sqrt(mahalanobis(z, center=cv$mean, cov=cv$varmat))
	limit <- sqrt(qchisq(p=quantile, df=ncol(x)-1))
	res <- list(mahalDist = dM, limit = limit, 
			    outlierIndex = dM > limit, method=method)
	class(res) <- "outCoDa"
    invisible(res)
}
