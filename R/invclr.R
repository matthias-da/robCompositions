invclr <- function(x, useClassInfo=TRUE){
	
	.Deprecated("cenLRinv")
	
	
	if(class(x) != "clr" & useClassInfo == TRUE) warning("useClassInfo was set to FALSE, because x is not from class clr")
	if(!(class(x) %in% c("clr", "data.frame", "matrix"))) stop("class from x must be either clr, data.frame or matrix")
	xclr <- x$x.clr
	if(class(x) == "clr" & useClassInfo==TRUE){
		dat <- exp(xclr)  * x$gm
	} else if(class(x) != "clr" | useClassInfo==FALSE){
		dat <- exp(x)  		
	}
	invisible(dat)	
}