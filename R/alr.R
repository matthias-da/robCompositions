alr <- function (x, ivar=ncol(x)){
	
	.Deprecated("addLR")
	
	if(dim(x)[2] < 2) stop("data must be of dimension greater equal 2")
	x.alr <- log(x/x[, ivar])
	res <- list(x.alr=x.alr[,-ivar], 
			varx=x[,ivar], ivar=ivar, cnames=colnames(x))
	class(res) <- "alr"
	invisible(res)
}
