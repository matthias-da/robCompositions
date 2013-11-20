print.pcaCoDa <- function(x, ...){
	## percentage of explained variability for clr transformed data
	eV <- x$eigenvalues / sum(x$eigenvalues)
	eVcum <- cumsum(x$eigenvalues) / sum(x$eigenvalues)
	cat("\n-------------------")
	cat("\n Percentages of explained variability for compositional data after clr transformation \n")
	print(eVcum)
	cat("\n-------------------\n\n")	
}