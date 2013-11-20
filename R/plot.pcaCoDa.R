plot.pcaCoDa <- function(x, y, ...){
	## biplot
	#z <- list()
	#z$scores <- x$scores
	#z$loadings <- x$loadings
	beschx <- if(x$method == "robust") "PC1 (clr-robust)" else "PC1 (clr-standard)"
	beschy <- if(x$method == "robust") "PC2 (clr-robust)" else "PC2 (clr-standard)"
#	biplot(x$princompOutputClr, xlab=beschx, ylab=beschy)
	biplot(x$princompOutputClr, xlab=beschx, ylab=beschy, ...)
}