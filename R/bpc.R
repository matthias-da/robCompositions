#' bpc
#' 
#' @title Backwards pivot coordinates and their inverse
#' @author Kamila Facevicova
#' @references Hron, K., Coenders, G., Filzmoser, P., Palarea-Albaladejo, J., Famera, M., Matys Grygar, M. (2022). Analysing pairwise logratios revisited. Mathematical Geosciences 53, 1643 - 1666.
#' @references Nesrstova, V., Jaskova, P., Pavlu, I., Hron, K., Palarea-Albaladejo, J., Gaba, A., Pelclova, J., Facevicova, K. (2023). Simple enough, but not simpler: Reconsidering additive logratio coordinates in compositional analysis. Submitted
#' @description Backwards pivot coordinate representation of a set of compositional ventors as a special case of isometric logratio coordinates and their inverse mapping.
#' 
#' @param X object of class data.frame. Positive values only.
#' @param base a positive number: the base with respect to which logarithms are computed. Defaults to exp(1).
#'
#' @details Backwards pivot coordinates map D-part compositional data from the simplex into a (D-1)-dimensional real space isometrically. The first coordinate has form of pairwise logratio log(x2/x1) and serves as an alternative to additive logratio transformation with part x1 being the rationing element. The remaining coordinates are structured as detailed in Nesrstova et al. (2023). 
#' Consequently, when a specific pairwise logratio is of the main interest, the respective columns have to be placed at the first (the compositional part in denominator of the logratio, the rationing element) and the second position (the compositional part in numerator) in the data matrix X.   
#' @keywords multivariate coordinates
#' @export
#' @seealso 
#' \code{\link{bpcTab}} 
#' \code{\link{bpcTabWrapper}} 
#' \code{\link{bpcPca}}
#' \code{\link{bpcReg}}
#' @return 
#' \item{Coordinates}{array of orthonormal coordinates.} 
#' \item{Coordinates.ortg}{array of orthogonal coordinates (without the normalising constant sqrt(i/i+1).} 
#' \item{Contrast.matrix}{contrast matrix corresponding to the orthonormal coordinates.}
#' \item{Base}{the base with respect to which logarithms are computed.}
#' \item{Levels}{the order of compositional parts.}
#' @examples 
#' data(expenditures)
#' 
#' # default setting with ln()
#' bpc(expenditures)
#' 
#' # logarithm of base 2
#' bpc(expenditures, base = 2)

bpc <- function(X, base = exp(1))
{ 
  D <- ncol(X)
  
  contrast.matrix <- matrix(NA, D-1, D)
  for(i in 1:(D-1))
    # contrast.matrix[i,] <- compositions::normalize(c(rep(-1/i, i), 1, rep(0, (D-i-1)))) 
    contrast.matrix[i,] <- norm1(c(rep(-1/i, i), 1, rep(0, (D-i-1))))
  rownames(contrast.matrix) <- paste("bpc", c(1:(D-1)), sep = ".")
  colnames(contrast.matrix) <- colnames(X)
  
  norm.const <- apply(contrast.matrix, 1, function(x) {r = sum(x > 0); s = sum(x < 0); sqrt(r*s/(r+s))})
  names(norm.const) <- paste("bpc", c(1:(D-1)), sep = ".")
  
  log.X <- log(data.matrix(X), base = base)
  bpc <- log.X %*% t(contrast.matrix)
  bpc.og <- t(t(bpc)/norm.const)
  
  message(paste0("The first backwards pivot coordinate (bpc.1) corresponds to the '", colnames(X)[2], " to ", colnames(X)[1], "' ratio."))
  
  return(list("Coordinates" = bpc, "Coordinates.ortg" = bpc.og, 
              "Contrast.matrix" = contrast.matrix, "Base" = base,
              "Levels" = colnames(X)))
}

