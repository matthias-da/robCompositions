#orthbasis <- function(D){
#		V <- matrix(0,nrow=D,ncol=D-1)
#		for (i in 1:ncol(V)){
#			V[1:i,i] <- 1/i
#			V[i+1,i] <- (-1)
#			V[,i] <- V[,i]*sqrt(i/(i+1))
#		}
#		return(V)
#}

orthbasis <- function(D){
	ilrBase <- NULL
	gsicomp <- 
			function (W = c(1, -1)) 
	{
		## this function is a copy of function gsi.buildilr* from 
		## package compositions. The authors are 
		## Raimon Tolosana-Delgado, K.Gerald v.d. Boogaart
		## and the intellecutal properties belongs to them.
		if (length(W) < 2) {
			return(ilrBase(D = 1))
		}
		if (length(dim(W)) == 0) {
			return(ilrBase(D = 2))
		}
		if (length(dim(W)) > 0) {
			W = as.matrix(W)
			nc = ncol(W)
			D = nrow(W)
			isPos = (W > 0)
			isNeg = (W < 0)
			nPos = matrix(1, D, D) %*% isPos
			nNeg = matrix(1, D, D) %*% isNeg
			W = (isPos * nNeg - isNeg * nPos)
			nn = sapply(1:nc, function(i) {
						1/sqrt(W[,i] %*% W[,i]) 
					})
			nn = matrix(nn, ncol = ncol(W), nrow = nrow(W), byrow = TRUE)
			W = W * nn
			return(W)
		}
	}
	codes=matrix(rep(0,(D-1)*D),ncol=D)
	for(i in 1:(D-1)){
		for(j in 1:D){
			codes[i,]=c(rep(0,i-1),-1,rep(1,D-i))
		}
	}   
	t(codes)	
	V <- gsicomp(t(codes))
	return(V)
}
