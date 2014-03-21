pcaCoDa <- function(x, method="robust",mult_comp=NULL){
  
  # Closure problem with ilr transformation
  ilrV <- function(x){
    # ilr transformation
    x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
    for (i in 1:ncol(x.ilr)){
      x.ilr[,i]=sqrt((i)/(i+1))*log(((apply(as.matrix(x[,1:i]), 1, prod))^(1/i))/(x[,i+1]))
    }
    return(x.ilr)
  }
  if(is.null(mult_comp)){
    xilr <- ilrV(x)
  }else{
    xilr <- do.call("cbind",lapply(mult_comp,function(xx)ilrV(x[,xx])))
  }		
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
  if(is.null(mult_comp)){
    V <- matrix(0, nrow=ncol(x), ncol=ncol(x)-1)
    for( i in 1:ncol(V) ){
      V[1:i,i] <- 1/i
      V[i+1,i] <- (-1)
      V[,i] <- V[,i]*sqrt(i/(i+1))
    }
  }else{
    V <- matrix(0, nrow=length(unlist(mult_comp)), ncol=length(unlist(mult_comp))-length(mult_comp))
    l <- sapply(mult_comp,length)
    start <- c(1,cumsum(l[-length(l)]))
    cumsum(l[-length(l)])
    end <- cumsum(l-1)
    start2 <- c(1,cumsum(l[-length(l)])+1)
    end2 <- cumsum(l)
    for(j in 1:length(mult_comp)){
      ind <- start[j]:end[j]
      ind2 <- start2[j]:end2[j]
      for( i in 1:length(ind)){
        V[ind2[1:i],ind[i]] <- 1/i
        V[ind2[i]+1,ind[i]] <- (-1)
        V[,ind[i]] <- V[,ind[i]]*sqrt(i/(i+1))
      }
    }
  }
  
 
  
  
  # robust ilr result - back-transformed to clr-space
  
  loadings <- V %*% pcaIlr$loadings	
  if(is.null(mult_comp)){
    if(!is.null(names(x))) dimnames(loadings)[[1]] <- names(x)
  }else{
    if(!is.null(names(x))) dimnames(loadings)[[1]] <- colnames(x)[unlist(mult_comp)]
  }
  pcaClr <- pcaIlr
#	pcaClr$scores <- pcaIlr$scores %*% t(V)
  pcaClr$scores <- pcaIlr$scores 
  pcaClr$loadings <- loadings
  
  res <- list(scores = pcaClr$scores,
      loadings = loadings,
      eigenvalues = eigenvalues,
      method = method,
      princompOutputClr = pcaClr,
      mult_comp = mult_comp
  )
  class(res) <- "pcaCoDa"
  invisible(res)
  
}