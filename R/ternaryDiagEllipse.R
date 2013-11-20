ternaryDiagEllipse <- function(x, tolerance=c(0.9,0.95,0.975), locscatt="MCD", ...){
	z <- isomLR(x)
	if(locscatt=="MCD"){
		cv <- covMcd(z)
		mu <- cv$center
		cm <- cv$cov
	} else {
		mu <- colMeans(z)
		cm <- cov(z)
	}
	dat1 <- drawMahal(z, mu, cm, plot=FALSE, whichlines=tolerance) 
	for(i in 1:length(tolerance)){
		e <- isomLRinv(cbind(dat1$mdX[,i], dat1$mdY[,i]))
		xp1 <- e[, 2] + e[, 3]/2
		yp1 <- e[, 3] * sqrt(3)/2	  
		lines(xp1, yp1, xlim = c(0, 1), ylim = c(0, 0.9), #frame.plot = FALSE, 
				xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
	}
}