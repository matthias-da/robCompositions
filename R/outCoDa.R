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