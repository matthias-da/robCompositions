pcaCoDa <- function(x, method="robust"){
	
	  # Closure problem with ilr transformation
		ilrV <- function(x){
	  # ilr transformation
			x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
			for (i in 1:ncol(x.ilr)){
				x.ilr[,i]=sqrt((i)/(i+1))*log(((apply(as.matrix(x[,1:i]), 1, prod))^(1/i))/(x[,i+1]))
			}
			return(x.ilr)
		}
		
	
	xilr <- ilrV(x)
		
	if( method == "robust"){
		cv <- covMcd(xilr, cor=FALSE)
		pcaIlr <- suppressWarnings(princomp(xilr, covmat=cv, cor=FALSE))
		eigenvalues <- eigen(cv$cov)$values
	} else if (method =="mve"){
		cv <- cov.mve(xilr)
		pcaIlr <- suppressWarnings(princomp(xilr, covmat=cv, cor=FALSE))
		eigenvalues <- eigen(cv$cov)$values
	} else {
		pcaIlr <- princomp(xilr, cor=FALSE)
		eigenvalues <- eigen(cov(xilr))$values
	}
	# construct orthonormal basis
	V <- matrix(0, nrow=ncol(x), ncol=ncol(x)-1)
	for( i in 1:ncol(V) ){
		V[1:i,i] <- 1/i
		V[i+1,i] <- (-1)
		V[,i] <- V[,i]*sqrt(i/(i+1))
	}
	
	
	# robust ilr result - back-transformed to clr-space
	
	loadings <- V %*% pcaIlr$loadings	
	if(!is.null(names(x))) dimnames(loadings)[[1]] <- names(x)
	
	pcaClr <- pcaIlr
#	pcaClr$scores <- pcaIlr$scores %*% t(V)
	pcaClr$scores <- pcaIlr$scores 
	pcaClr$loadings <- loadings
	
	res <- list(scores = pcaClr$scores,
			    loadings = loadings,
				eigenvalues = eigenvalues,
				method = method,
				princompOutputClr = pcaClr
				)
	class(res) <- "pcaCoDa"
	invisible(res)
	
}