#' Correlations for compositional data
#' 
#' This function computes correlation coefficients between compositional parts based
#' on symmetric pivot coordinates.
#' 
#' @param x a matrix or data frame with compositional data
#' @param ... additional arguments for the function \code{\link{cor}}
#' @return A compositional correlation matrix.
#' @author Petra Kynclova
#' @export
#' @examples
#' data(expenditures)
#' corCoDa(expenditures)
#' @references Kynclova, P., Hron, K., Filzmoser, P. (2017)
#' Correlation between compositional parts based on symmetric balances.
#' \emph{Mathematical Geosciences}, 49(6), 777-796.
#' @examples
#' x <- arcticLake 
#' corCoDa(x)
#' 

corCoDa <- function(x, ...){
  
  # check
  if(!is.matrix(x) & !is.data.frame(x)) stop("x must be a matrix or data.frame")
  if(any(x[!is.na(x)]<=0)) stop("all elements of x must be greater than 0")
  if(ncol(x)<=2) stop("calculation of average symmetric coordinates not possible")
  
  balZav <- function(x){
    D <- ncol(x)
    Z.av <- matrix(NA,ncol=2,nrow=nrow(x))
    p1 <- sqrt(D-1+sqrt(D*(D-2)))/sqrt(2*D)
    
    if(D==3){
      p2 <- x[,3]
    }
    else{
      p2 <- apply(x[,3:D],1,prod)
    }
    
    p3 <- (sqrt(D-2)+sqrt(D))/(sqrt(D-2)*(D-1+sqrt(D*(D-2))))
    p4 <- 1/(D-1+sqrt(D*(D-2)))
    Z.av[,1] <- p1*(log(x[,1]/(x[,2]^p4 * p2^p3)))
    Z.av[,2] <- p1*(log(x[,2]/(x[,1]^p4 * p2^p3)))
    return(Z.av)
  }
  
  ind <- c(1:ncol(x))
  corZav <- matrix(NA,ncol(x),ncol(x))
  for (i in 1:(ncol(x)-1)){
    for (j in (i+1):ncol(x)){
      corZav[i,j] <- cor(balZav(x[,c(i,j,ind[-c(i,j)])]), ...)[1,2] # correlations for average coordinates Z.av
    }
  }
  corZav[lower.tri(corZav)] <- t(corZav)[lower.tri(corZav)]
  diag(corZav) <- 1
  return(corZav)
}
