clr <- function(x){
	
	.Deprecated("cenLR")
	
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
	x.clr <- log(x/gm)
	res <- list(x.clr=x.clr, 
			    gm=gm
		   )
	}
	class(res) <- "clr"
	invisible(res)  
}