print.daFisher <- function(x,...){
	cat("--------------------------------------")
	cat("\nResults from Fishers discriminant analysis, coda ==", x$coda)
	cat("\n- Variance between the classes: \n")
	print(x$B)
	cat("\n- Variance within the classes: \n")
	print(x$W)
	cat("\n- Loadings matrix: \n")
	print(x$load)
	cat("--------------------------------------\n")
}