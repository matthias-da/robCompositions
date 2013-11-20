summary.adtestWrapper=function(object,...){
	d=data.frame(ilrVars=unlist(object$info),
			testName=unlist(lapply(object$res,function(x) x$method)),
			testStat=unlist(lapply(object$res,function(x) x$statistic)),
			pvalue=unlist(lapply(object$res,function(x) x$p.value)),
			#alpha=object$alpha,
			check=object$check)
#d=lapply(res,function(x) x$info)
	string <- paste("Anderson-Darling test results ( alpha =", object$alpha, "):")
	string2 <- paste("--> p-values and tests are obtained from", object$est, "estimates.")
	cat("\n  -----------------------------------------------")	
	cat("\n ", string)
	cat("\n  ----------------\n")
	print(d)
	cat("\n  -----------------------------------------------\n")
	cat("\n ", string2)
	cat("\n")
	invisible(d)
}