"isomLR" <- function(x){
	x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
	D=ncol(x)
	for (i in 1:ncol(x.ilr)){
#		x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
		x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(apply(as.matrix(x[,(i+1):D]), 1, gm)/(x[,i]))	
	} 
	return(x.ilr)
}
