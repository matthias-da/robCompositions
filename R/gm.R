gm <- function (x) {
	if(!is.numeric(x)) stop("x has to be a vector of class numeric")
	if (any(na.omit(x == 0)))
		0
	else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
}