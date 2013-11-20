drawMahal <- 
		function(x,x.mean,x.cov,whichlines=c(0.975,0.90,0.75),m=360,plot=TRUE,
				xlab="",ylab="",cex.lab=1.2, ...){
# draw ellipse corresponding to Mahalanobis distances
# PF, 06.12.2006
#
# x ... data
# x.mean ... mean of data
# x.cov ... cov-matrix of data
# whichlines: lines corresponding to quantiles of chi2
# m: number of data points on the ellipse
# plot ... if plot=TRUE the lines are plotted, otherwise not
#
	
# OUTPUT
# objects "mdX" and "mdY" with m rows and length(whichlines) columns
# (the columns refer to X- and Y-coordinates of one ellipse)
	
	mdX=matrix(NA,nrow=m,ncol=length(whichlines))
	mdY=matrix(NA,nrow=m,ncol=length(whichlines))
	
	cov.svd <- svd(x.cov, nv = 0)
	r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
	
	alphamd <- sqrt(qchisq(whichlines,2))
	
	for (j in 1:length(whichlines)){
		e1md <- cos(c(0:(m-1))/m * 2 * pi) * alphamd[j]
		e2md <- sin(c(0:(m-1))/m * 2 * pi) * alphamd[j]
		emd <- cbind(e1md, e2md)
		ttmd <- t(r %*% t(emd)) + rep(1, m) %o% x.mean
		
		mdX[,j] <- ttmd[,1]
		mdY[,j] <- ttmd[,2]
		
		if (plot) lines(mdX[,j],mdY[,j], type = "l", ...)
	}
	
	list(mdX=mdX,mdY=mdY)
}

impAll <-
		function(x) {
	maxLimits = apply(x, 2, min, na.rm = TRUE)
	maxLimits = -maxLimits
	maxLimits[maxLimits < 0] = 0
	
	if(any(is.na(x))) {
		temp <- x
		for(i in 1:length(maxLimits)) {
			temp[na.omit(temp[i]) < 0, i] = maxLimits[i] * 2 / 3
		}
		
		res <- adjust(impCoda(temp))
		isna = is.na(x)
		x[isna] = res$xImp[isna]
	}
	if(any(x < 0)) {
		temp <- x
		temp[temp < 0] = 0
		
		res <- impRZilr(temp, dl=maxLimits)
		x = res$xImp
	}
	
	return(x)
}


