#' Pivot coordinates and their inverse
#' 
#' An isometric log-ratio transformation and it's inverse transformation with a 
#' special choice of the balances
#' 
#' This transformation moves D-part compositional data from the simplex
#' into a (D-1)-dimensional real space isometrically. From our choice of (pivot)
#' balances, all the relative information of one part is seperated from the
#' remaining parts. It is useful, e.g. for estimating missing values in
#' \eqn{x_1} by regression of the remaining variables.
#' 
#' @aliases pivotCoord isomLR isomLRinv isomLRp isomLRinvp
#' @param x object of class data.frame or matrix. Positive values only.
#' @param pivotvar pivotal variable. If any other number than 1, the data are resorted in 
#' that sense that the pivotvar is shifted to the first part.
#' @param fast if TRUE, it is approx. 10 times faster but numerical problems in case of 
#' high-dimensional data numerical instabilities may occur.
#' @param base a positive or complex number: 
#' the base with respect to which logarithms are computed. Defaults to \code{exp(1)}.
#' @param norm if FALSE then the normalizing constant is not used, if TRUE \code{sqrt((D-i)/(D-i+1))} is 
#' used (default). The user can also specify a self-defined constant.
#' @return The data represented in pivot coordinates
#' @author Matthias Templ, Karel Hron
#' @references Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and C.
#' Barcel'o-Vidal (2003) Isometric logratio transformations for compositional
#' data analysis. \emph{Mathematical Geology}, \bold{35}(3) 279-300. \
#' 
#' Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values
#' for compositional data using classical and robust methods
#' \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages
#' 3095-3107.
#' @keywords math
#' @export
#' @examples
#' 
#' require(MASS)
#' Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)
#' z <- isomLRinv(mvrnorm(100, mu=c(0,2), Sigma=Sigma))
#' 
#' data(expenditures)
#' ## first variable as pivot variable
#' pivotCoord(expenditures)
#' ## third variable as pivot variable
#' pivotCoord(expenditures, 3) 
#' 
#' x <- exp(mvrnorm(2000, mu=rep(1,10), diag(10)))
#' system.time(pivotCoord(x))
#' system.time(pivotCoord(x, fast=TRUE))
#' 
#' ## without normalizing constant
#' pivotCoord(expenditures, norm = "1")
#' ## other normalization
#' pivotCoord(expenditures, norm = "-sqrt((D-i)/(D-i+1))")
#' 
pivotCoord <- function(x, pivotvar = 1, fast=FALSE, base = exp(1), norm = "sqrt((D-i)/(D-i+1))"){
  if(dim(x)[2] < 2) stop("data must be of dimension greater equal 2")
  if(any(x < 0)) stop("negative values not allowed")
  x.ilr <- matrix(NA, nrow = nrow(x), ncol = ncol(x) - 1)
  D <- ncol(x)
  ## order parts according to pivotvar
  w <- which(pivotvar != 1:D)
  x <- x[, c(pivotvar, w)]
  if(fast){
    for (i in 1:ncol(x.ilr)){
      x.ilr[,i] <- eval(parse(text = norm)) * log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]), base)
    }
  } else {
    for (i in 1:ncol(x.ilr)){
      #		x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
      x.ilr[,i] <- eval(parse(text = norm)) * log(apply(as.matrix(x[,(i+1):D]), 1, gm)/(x[,i]), base)	
    } 
  }
  if(is.data.frame(x)) x.ilr <- data.frame(x.ilr)
  if(all(nchar(colnames(x)) > 1)){
    for(i in 1:(D-1)){
      colnames(x.ilr)[i] <- paste(colnames(x)[i], "_", paste(substr(colnames(x[(i+1):D]), 1, 2), collapse="-"), collapse="", sep="")
    }
  }  
  return(-x.ilr)
}

#' @rdname pivotCoord
#' @export
isomLR <- function(x, fast=FALSE, base = exp(1), norm = "sqrt((D-i)/(D-i+1))"){
    .Deprecated("pivotCoord") 
    pivotCoord(x = x, fast=fast, base = base, norm = norm)
}

#' @rdname pivotCoord
#' @export
isomLRinv <- function(x){
  .Depricated("pivotCoordInv")
  pivotCoordInv(x = x)
}

#' @rdname pivotCoord
#' @export
pivotCoordInv <- function(x){
  x <- -x
  y <- matrix(0, nrow=nrow(x), ncol=ncol(x)+1)
  D <- ncol(x)+1
  y[,1] <- -sqrt((D-1)/D)*x[,1]
  for (i in 2:ncol(y)){
    for (j in 1:(i-1)){
      y[,i]=y[,i]+x[,j]/sqrt((D-j+1)*(D-j))
    }
  }
  for (i in 2:(ncol(y)-1)){
    y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x[,i]
  }
  yexp=exp(y)
  x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
  if(is.data.frame(x)) x.back <- data.frame(x.back)
  return(x.back)
  #return(yexp)
}
#' @rdname pivotCoord
#' @export
isomLRp <- function(x, fast=FALSE, base = exp(1), norm = "sqrt((D-i)/(D-i+1))"){
  x.ilr <- matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
  D <- ncol(x)
  if(fast){
    for (i in 1:ncol(x.ilr)){
      x.ilr[,i] <- eval(parse(text = norm))  * log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]), base)
    }
  } else {
    for (i in 1:ncol(x.ilr)){
      #		x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
      x.ilr[,i] <- eval(parse(text = norm))  * log(apply(as.matrix(x[,(i+1):D]), 1, gm)/(x[,i]), base)	
    } 
  }
  if(is.data.frame(x)) x.ilr <- data.frame(x.ilr)
  return(x.ilr)
}
#' @rdname pivotCoord
#' @export
isomLRinvp <- function(x){
  y=matrix(0,nrow=nrow(x),ncol=ncol(x)+1)
  D=ncol(x)+1
  y[,1]=-sqrt((D-1)/D)*x[,1]
  for (i in 2:ncol(y)){
    for (j in 1:(i-1)){
      y[,i]=y[,i]+x[,j]/sqrt((D-j+1)*(D-j))
    }
  }
  for (i in 2:(ncol(y)-1)){
    y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x[,i]
  }
  yexp=exp(y)
  x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
  if(is.data.frame(x)) x.back <- data.frame(x.back)
  return(x.back)
  #return(yexp)
}
