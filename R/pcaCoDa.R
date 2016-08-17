#' Robust principal component analysis for compositional data
#' 
#' This function applies robust principal component analysis for compositional
#' data.
#' 
#' The compositional data set is transformed using the ilr tranformation.
#' Afterwards, robust principal component analysis is performed.  Resulting
#' loadings and scores are back-transformed to the clr space where the
#' compositional biplot can be shown.
#' 
#' \code{mult_comp} is used when there are more than one group of compositional
#' parts in the data. To give an illustrative example, lets assume that one
#' variable group measures angles of the inner ear-bones of animals which sum
#' up to 100 and another one having percentages of a whole on the thickness of
#' the inner ear-bones included. Then two groups of variables exists which are
#' both compositional parts. The ilr-transformation is then internally applied
#' to each group independently whenever the \code{mult_comp} is set correctly.
#' 
#' @aliases pcaCoDa print.pcaCoDa 
#' @param x compositional data
#' @param method must be either \dQuote{robust} (default) or \dQuote{classical}
#' @param mult_comp a list of numeric vectors holding the indices of linked
#' compositions
#' @param external external non-compositional variables
#' @return \item{scores }{scores in clr space} \item{loadings }{loadings in clr
#' space} \item{eigenvalues }{eigenvalues of the clr covariance matrix}
#' \item{method }{method} \item{princompOutputClr }{output of \code{princomp}
#' needed in \code{plot.pcaCoDa}}
#' @author K. Hron, P. Filzmoser, M. Templ
#' @seealso \code{\link{print.pcaCoDa}}, \code{\link{summary.pcaCoDa}}, \code{\link{biplot.pcaCoDa}}, \code{\link{plot.pcaCoDa}}
#' @importFrom stats princomp
#' @references Filzmoser, P., Hron, K., Reimann, C. (2009) Principal Component
#' Analysis for Compositional Data with Outliers. \emph{Environmetrics},
#' \bold{20}, 621-632.
#' 
#' Kynclova, P., Filzmoser, P., Hron, K. (2016) Compositional biplots including external non-compositional variables. 
#' \emph{Statistics: A Journal of Theoretical and Applied Statistics},
#' \bold{50}, 1132-1148.
#' @keywords multivariate
#' @export
#' @importFrom MASS cov.mve
#' @examples
#' 
#' data(arcticLake)
#' 
#' ## robust estimation (default):
#' res.rob <- pcaCoDa(arcticLake)
#' res.rob
#' summary(res.rob)
#' plot(res.rob)
#' 
#' ## classical estimation:
#' res.cla <- pcaCoDa(arcticLake, method="classical")
#' biplot(res.cla)
#' 
#' ## just for illustration how to set the mult_comp argument:
#' data(expenditures)
#' p1 <- pcaCoDa(expenditures, mult_comp=list(c(1,2,3),c(4,5)))
#' p1
#' 
#' ## example with external variables:
#' data(election)
#' # transform external variables
#' election$unemployment <- log((election$unemployment/100)/(1-election$unemployment/100))
#' election$income <- scale(election$income)
#' 
#' res <- pcaCoDa(election[,1:6], method="classical", external=election[,7:8])
#' res
#' biplot(res, scale=0)

pcaCoDa <- function(x, method="robust", mult_comp=NULL, external=NULL){
 
  if(is.vector(external) & length(external)!=nrow(x)){ 
      stop("external and x must have the same number of observations")
  }
  if(!is.null(mult_comp) & !is.list(mult_comp)) stop("if specified, mult_comp must be a list")
  
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
  if(!is.null(external)){
    xilr <- cbind(xilr, external)
  }
  if( method == "robust"){
    cv <- robustbase::covMcd(xilr, cor=FALSE)
    pcaIlr <- suppressWarnings(princomp(xilr, covmat=cv, cor=FALSE))
    eigenvalues <- eigen(cv$cov)$values
  } else if (method =="mve"){
    cv <- MASS::cov.mve(xilr)
    pcaIlr <- suppressWarnings(princomp(xilr, covmat=cv, cor=FALSE))
    eigenvalues <- eigen(cv$cov)$values
  } else {
    pcaIlr <- princomp(xilr, cor=FALSE)
    eigenvalues <- eigen(cov(xilr))$values
  }
  # construct orthonormal basis
  if(is.null(mult_comp)){
    #V <- matrix(0, nrow=ncol(x), ncol=ncol(x)-length(external)-1)
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
  if(!is.null(external)){
    nload <- nrow(pcaIlr$loadings)
    if(dim(external)[2] < 2) index <- 1
    else index <- ncol(external)
    loadings <- V %*% pcaIlr$loadings[-c((nload-index+1):nload),] # transform without external loadings
    loadings <- rbind(loadings, pcaIlr$loadings[(nload-index+1):nload,]) 
  }
  else{
    loadings <- V %*% pcaIlr$loadings
  }
  if(is.null(mult_comp)){
    if(!is.null(names(x)) & !is.null(external)) dimnames(loadings)[[1]] <- c(names(x), names(external))
    else if(!is.null(names(x))) dimnames(loadings)[[1]] <- names(x)
  }else{
    if(!is.null(names(x))) dimnames(loadings)[[1]] <- colnames(x)[unlist(mult_comp)]
  }
  pcaClr <- pcaIlr
#  pcaClr$scores <- pcaIlr$scores %*% t(V)
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

#' @rdname pcaCoDa
#' @export
#' @method print pcaCoDa
#' @param ... additional parameters for print method passed through
print.pcaCoDa <- function(x, ...){
  ## percentage of explained variability for clr transformed data
  eV <- x$eigenvalues / sum(x$eigenvalues)
  eVcum <- cumsum(x$eigenvalues) / sum(x$eigenvalues)
  cat("\n-------------------")
  cat("\n Percentages of explained variability for compositional data \n after clr transformation \n")
  print(eVcum)
  cat("\n-------------------\n\n")	
}

#' @rdname pcaCoDa
#' @method summary pcaCoDa
#' @param object object of class pcaCoDa
#' @export

summary.pcaCoDa <- function(object, ...){
    stopifnot(inherits(object, "pcaCoDa"))
    summary(object$princompOutputClr)
}
