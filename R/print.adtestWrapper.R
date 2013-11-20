print.adtestWrapper <- function(x,...){
	if(all(x$check)){
		print(paste("The data follow the normal distribution on the simplex (alpha =",x$alpha,")",sep=""))
	} else { 
		print(paste("The data do not follow the normal distribution on the simplex (alpha =",x$alpha,")",sep=""))
		#print(x$check)
}
}