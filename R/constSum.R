constSum <- function(x, const=1, na.rm=TRUE){
	return(x / rowSums(x, na.rm) * const)
}