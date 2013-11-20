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