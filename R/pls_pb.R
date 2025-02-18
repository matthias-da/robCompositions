#' Function calculating a set of (D-1) principal balances based on PLS.
#'
#' @param Xcoda a matrix of raw compositional data with "n" rows and "D" columns/components
#' @param ycoda a response variable; can be continuous (PLS regression) or binary (PLS-DA)
#' @param version a parameter determining whether the balances are ordered according to max. covariance (default) or max. correlation
#'
#' @importFrom pls plsr
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{bal}}{A matrix of (D-1) principal balances.}
#'   \item{\code{cov}}{Covariance of each balance with the response variable.}
#' }
#'
#' @details
#' The function creates a set of (D-1) principal balances based on PLS. The procedure builds on the method building principal balances based on PCA, introduced in Martin-Fernandez et al. (2018)
#' For detailed information regarding PLS principal balances, see Nesrstová et al. (2023).
#'
#' @author Viktorie Nesrstová
#'
#' @references
#' J. A. Martín-Fernández, V. Pawlowsky-Glahn, J. J. Egozcue, and R. Tolosona-Delgado. Advances in principal balances for compositional data. Mathematical Geosciences, 50(3):273–298, 2018. Available at:
#' \url{https://link.springer.com/article/10.1007/s11004-017-9712-z}
#' DOI: \doi{10.1007/s11004-017-9712-z}
#'
#' Nesrstová, V, Wilms, I, Palarea-Albaladejo, J, et al. Principal balances of compositional data for regression and classification using partial least squares. Journal of Chemometrics. 2023; 37(12):e3518..  Available at:
#' \url{https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/full/10.1002/cem.3518}
#' DOI: \doi{10.1002/cem.3518}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   if (requireNamespace("MASS", quietly = TRUE)) {
#'
#' # 1. Generate sample data ------------------------------------------------------
#' n <- 100              # observations
#' D <- 15               # parts/variables
#' Sig <- diag(D-1)      # positive-definite symmetric matrix -> covariance matrix
#' mu <- c(rep(0, D-1))  # means of variables
#'
#' set.seed(123)
#' # ilr coordinates
#' Z <- MASS::mvrnorm(n,mu,Sigma = Sig)
#'
#' # Z -> CoDa X
#' V <- compositions::ilrBase(D = D)  # ilrBase() in library(compositions)
#' X <- as.matrix(as.data.frame(acomp(exp(Z%*%t(V)))))
#'
#' # Response y:
#' beta <- runif(D-1,0.1,1)
#' eps <- rnorm(n)
#' y <- Z%*%beta+eps
#'
#' # 2. Calculate PLS PBs
#'
#' PLS_balances <- fBalChip_PLS(X,y,version = "cov")     # version = "cov" -> max. covariance
#' balances <- PLS_balances$bal
#'   }
#' }
#'
pls_pb <-function(Xcoda, ycoda, version = "cov"){

  numbal=ncol(Xcoda)-1

  # call the recursive function
  res<-fBPMaxOrthNewChip_PLS(Xcoda,ycoda,version=version)
  Bres<-res$bal
  balname<-paste("bal",1:nrow(Bres),sep="")
  rownames(Bres)<-balname
  colnames(Bres)<-colnames(Xcoda)
  Vres<-res$varbal

  # sort by expl var
  vopt<-res$varbal
  # sort variance
  vsopt<-sort(vopt,decreasing = TRUE,index.return=TRUE)
  #
  # assign variance explained already ordered
  Vres<-vsopt$x
  #
  # assign balances same order
  Bres<-Bres[vsopt$ix,]
  #
  # return results: balances and variances

  if(version=="cov"){
    return(list(bal=Bres,cov=Vres))
  } else if(version=="cor"){
    return(list(bal=Bres,cor=Vres))
  }
}

