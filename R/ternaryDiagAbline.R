ternaryDiagAbline <- function(x, ...){
	if(ncol(x) > 2) stop("it is assumed that the 2-dim points are provided in the transformed space.")
#	plot(x)
	k <- (x[2,2]-x[1,2])/(x[2,1]-x[1,1])
	a <- c(0,c(x[1,2]-k*(x[1,1])))
	fk <- function(x,a,k){
		y <- k*x+a[2]
		y
	}
	SEQ <- seq(-10,10,length=1000)
	x <- fk(SEQ,a,k)
#	print(x)
	x <- cbind(SEQ,x)
#	lines(x)
#	print(x)
	x <- isomLRinv(x)
	s <- rowSums(x)
	if (any(s <= 0)) 
		stop("rowSums of the input data x must be positive.")
	x <- x/s
	top <- sqrt(3)/2
	xp <- x[, 2] + x[, 3]/2
	yp <- x[, 3] * top
	lines(xp, yp, ...)	
}