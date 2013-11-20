adjust <- function(x){
	# x ... object from class "imp"
	if(class(x) != "imp") stop("object x must be from class imp")
	xneu=x$xImp
	s1 <- rowSums(x$xOrig, na.rm=TRUE)
	for(i in 1:nrow(x$xImp)){
		s <- sum(x$xImp[i, !x$wind[i,]])
		s2 <- sum(x$xImp[i, x$wind[i,]])
		fac <- s / (s + s2)
		s1[i] <-  s1[i] / fac
	}
	impS <- s1/rowSums(x$xImp)
	for(i in 1:ncol(x$xImp)){
		xneu[,i] <- x$xImp[,i] * impS
	}
	x$xImp <- xneu
	invisible(x)
}