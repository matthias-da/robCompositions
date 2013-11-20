"invilr" <- function(x.ilr){
	
	.Deprecated("isomLRinv")
		
	y=matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
	D=ncol(x.ilr)+1
	y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
	for (i in 2:ncol(y)){
	   for (j in 1:(i-1)){
	      y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
	   }
	}
	for (i in 2:(ncol(y)-1)){
	   y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
	}
	yexp=exp(y)
	x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
	invisible(x.back)
	#return(yexp)
}
