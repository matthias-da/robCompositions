#' @title relative difference between covariance matrices
#' @name rdcm
#' @description The sample covariance matrices are computed with the same isometric transformed observations. 
#' @param x matrix or data frame
#' @param y matrix or data frame of the same size as x.
#' @return the error measures value
#' @author Matthias Templ
#' @references Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of
#' missing values for compositional data using classical and robust methods
#' \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages
#' 3095-3107.
#' Templ, M. and Hron, K. and Filzmoser and Gardlo, A. (2016). 
#' Imputation of rounded zeros for high-dimensional compositional data. 
#' \emph{Chemometrics and Intelligent Laboratory Systems}, 54 (12) 3095-3107.
#' 
#' @seealso \code{\link{rdcm}}
#' @keywords manip
#' @export
#' @details The difference in covariance structure is based on the Euclidean distance between both covariance estimations. 
#' @examples
#' data(expenditures)
#' x <- expenditures
#' x[1,3] <- NA
#' xi <- impKNNa(x)$xImp
#' rdcm(expenditures, xi)
rdcm <- function(x, y){
  ## from package matrixcalc, CRAN version 1.0.3
  fn <- 
  function (x) 
  {
    return(en(x, 2))
  }
  ## from package matrixcalc, CRAN version 1.0.3
  en <- 
  function (x, p) 
  {
    if (!is.numeric(x)) {
      stop("argument x is not numeric")
    }
    if (is.matrix(x)) {
      Xmat <- x
    }
    else {
      if (is.vector(x)) {
        Xmat <- matrix(x, nrow = length(x), ncol = 1)
      }
      else {
        stop("argument x is neither vector nor matrix")
      }
    }
    if (p == 0) {
      stop("exponent p is zero")
    }
    return((sum(abs(Xmat)^p))^(1/p))
  }
  # new code
  ocov <- cov(pivotCoord(x))
  rcov <- cov(pivotCoord(y))
  return(fn(ocov-rcov)/fn(ocov))
}
