addLRinv <- function(x, cnames=NULL, ivar=NULL, useClassInfo=TRUE){
	if(class(x) == "alr" & useClassInfo==TRUE){
		
		xalr <- x$x.alr
		ivar <- x$ivar
		dat <- exp(xalr)*x$varx
		## correct order of the variables:
		if(ivar == dim(xalr)[2]+1){ 
			dat <- cbind(dat, x$varx)
		} else if(ivar == 1){  
			dat <- cbind(x$varx, dat)			
		} else{
			dat <- cbind(dat[,1:(ivar-1)], x$varx, dat[,(ivar):(dim(xalr)[2])])
		}
		colnames(dat) <- x$cnames
		if(class(x$x.alr) == "data.frame") dat <- as.data.frame(dat)
	} else if(class(x)=="alr" & useClassInfo == FALSE){
		if(is.null(ivar)) stop("object ivar must be provided \n because object x is not from class alr")
		xalr <- x$x.alr
		#if(is.null(cnames)) cnames <- c(colnames(x), "rat")
		#if(length(cnames)==1) cnames <- paste("V", 1:dim(x)[2]+1, sep="")	
		#if(length(cnames) != dim(x)[2] + 1 | length(cnames) < 2) stop(paste("cnames must be of length", dim(x)[2]+1))
		rat <- rowSums(exp(xalr)) + 1 
		dat <- exp(xalr)/rat
		## correct order of the variables:
		if(ivar == dim(xalr)[2]+1){ 
			dat <- cbind(dat, 1/rat)
		} else if(ivar == 1){
			dat <- cbind(1/rat, dat)			
		} else{
			dat <- cbind(dat[,1:(ivar-1)], 1/rat, dat[,(ivar):(dim(xalr)[2])])
		}
		#colnames(dat) <- x$cnames
	} else if(class(x) != "alr"){
		if(dim(x)[2] < 2) stop("data must be of dimension greater equal 2")
		#if(useClassInfo) warning("x is not from class alr, absolute values are not preserved and column names may not be respected")
		if(is.null(ivar)){ 
			warning(paste("object ivar is not provided \n it is assigned to ", dim(x)[2]+1, sep=""))
			ivar <- dim(x)[2]+1
		}
		xalr <- x
		if(is.null(cnames)) cnames <- c(colnames(x), "rat")
		if(length(cnames)==1) cnames <- paste("V", 1:dim(x)[2]+1, sep="")	
		if(length(cnames) != dim(x)[2] + 1 | length(cnames) < 2) stop(paste("cnames must be of length", dim(x)[2]+1))
		rat <- rowSums(exp(xalr)) + 1 
		dat <- exp(xalr)/rat
		## correct order of the variables:
		if(ivar == dim(xalr)[2]+1){ 
			dat <- cbind(dat, 1/rat)
		} else if(ivar == 1){
			dat <- cbind(1/rat, dat)			
		} else{
			dat <- cbind(dat[,1:(ivar-1)], 1/rat, dat[,(ivar):(dim(xalr)[2])])
		}		
		colnames(dat) <- cnames
	}
	
	
	
	return(dat)
}

