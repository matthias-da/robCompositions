#' plot method for outCoDa objects
#' 
#' Plots the Mahalanobis distance.
#' 
#' The dashed line indicates the (1 - alpha) quantile of the Chi-squared
#' distribution. Observations with Mahalanobis distance greater than this
#' quantile could be considered as compositional outliers.
#' 
#' @param x object from class \sQuote{outCoDa}
#' @param y ...
#' @param \dots ...
#' @author Matthias Templ
#' @seealso \code{\link{outCoDa}}
#' @references Filzmoser, P., and Hron, K. (2008) Outlier detection for
#' compositional data using robust methods. \emph{Math. Geosciences}, \bold{40}
#' 233-248.
#' @keywords hplot
#' @examples
#' 
#' data(expenditures)
#' oD <- outCoDa(expenditures)
#' oD
#' plot(oD)
#' 
plot.outCoDa <- function(x, y, ...){
#	plot(1:length(x$mahalDist), x$mahalDist, ylab="Mahalanobis distance", xlab="Index", type="n", ylim=c(0, max(x$mahalDist)) )
#	points(1:length(x$mahalDist[x$outlierIndex]), x$mahalDist[x$outlierIndex], pch=3, col="red")
#	points(1:length(x$mahalDist[!x$outlierIndex]), x$mahalDist[!x$outlierIndex])	
#	abline(h=x$limit, lty=2)
#	#print(x$mahalDist)
	if(x$method =="standard") yl <- "Mahalanobis distance" else yl <- "Robust Mahalanobis distance" 
	plot(1:length(x$mahalDist), x$mahalDist, ylab = yl,
			xlab = "Index", ylim = c(0, max(x$mahalDist)),
			pch = as.numeric(x$outlierIndex)*2+1)
	abline(h=x$limit, lty=2)
}
