#' Robust and classical variation matrix
#' 
#' Estimates the variation matrix with robust methods.
#' 
#' The variation matrix is estimated for a given compositional data set.
#' Instead of using the classical standard deviations the miniminm covariance estimator
#' is used (\code{\link[robustbase]{covMcd}}) is used when parameter robust is set to TRUE.
#' 
#' @param x data frame or matrix with positive entries
#' @param method method used for estimating covariances. See details.
#' @details For method \code{robustPivot} forumala 5.8. of the book (see second reference) is used. Here 
#' robust (mcd-based) covariance estimation is done on pivot coordinates. 
#' Method \code{robustPairwise} uses a mcd covariance estimation on pairwise log-ratios.
#' Methods \code{Pivot} (see second reference) and \code{Pairwise} (see first reference) 
#' are the non-robust counterparts. 
#' Naturally, \code{Pivot} and \code{Pairwise} gives the same results, but 
#' the computational time is much less for method \code{Pairwise}.
#' @return The (robust) variation matrix.
#' @author Karel Hron, Matthias Templ
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman \& Hall Ltd., London (UK). 416p.
#' 
#' #' Filzmoser, P., Hron, K., Templ, M. (2018) \emph{Applied Compositional Data Analysis}.
#' Springer, Cham.
#' @keywords multivariate robust
#' @export
#' @examples
#' 
#' data(expenditures)
#' variation(expenditures) # default is method "robustPivot"
#' variation(expenditures, method = "Pivot")
#' variation(expenditures, method = "robustPairwise")
#' variation(expenditures, method = "Pairwise") # same results as Pivot
#' 
`variation` <-
  function(x, method = "robustPivot"){
    stopifnot(method %in% c("robustPivot","Pivot","robustPairwise","Pairwise"))
    if(method == "robustPivot" | method == "Pivot"){
      n <- nrow(x)
      D <- ncol(x)
      z <- pivotCoord(x)
      if(method == "robustPivot"){
        CV <- covMcd(z)$cov
      } 
      if(method == "Pivot"){
        CV <- cov(z)
      }
      V <- orthbasis(ncol(x))$V
      #robust variation matrix according to Eq. (5.8) from the book Filzmoser et al. (2018)
      J <- matrix(1,ncol=D,nrow=D)
      rvars <- J%*%diag(diag(V%*%CV%*%t(V)))+diag(diag(V%*%CV%*%t(V)))%*%J-2*V%*%CV%*%t(V)
    }
    ## method robustPairwise:
    if(method == "robustPairwise"){
      rvars <- matrix(0, ncol=ncol(x), nrow=ncol(x))
      for( i in 1:ncol(x)){
        for( j in 1:ncol(x)){
          if( i < j ){
             #rvars[i,j] <- (mad(log(x[,i]/x[,j])))^2
            rvars[i,j] <- robustbase::covMcd(log(x[,i]/x[,j]))$cov
            rvars[j,i] <- rvars[i,j]
          }
        }
      }
    } else if(method == "Pairwise"){
      rvars <- matrix(0, ncol=ncol(x), nrow=ncol(x))
      for( i in 1:ncol(x)){
        for( j in 1:ncol(x)){
          if( i < j ){ 
            rvars[i,j] <- (var(log(x[,i]/x[,j])))
            rvars[j,i] <- rvars[i,j]            
          }
        }
      }		
    }
#    rvars <- data.frame(rvars)
    colnames(rvars) <- colnames(x)
    rownames(rvars) <- colnames(x)
    return(rvars) 
}


