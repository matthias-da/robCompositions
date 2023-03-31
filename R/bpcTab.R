#' bpcTab
#' 
#' @title Backwards pivot coordinates and their inverse
#' @importFrom dplyr arrange
#' @importFrom tidyr expand_grid
#' @author Kamila Facevicova
#' @references Nesrstova, V., Jaskova, P., Pavlu, I., Hron, K., Palarea-Albaladejo, J., Gaba, A., Pelclova, J., Facevicova, K. (2023). Simple enough, but not simpler: Reconsidering additive logratio coordinates in compositional analysis. Submitted
#' @description Backwards pivot coordinate representation of a compositional table as a special case of isometric logratio coordinates and their inverse mapping.
#' 
#' @param x object of class data.frame with columns corresponding to row and column factors of the respective compositional table and a variable with the values of the composition (positive values only).
#' @param row.factor name of the variable representing the row factor. Needs to be given with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be given with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be given with the quotation marks.
#' @param base a positive number: the base with respect to which logarithms are computed. Defaults to exp(1).
#'
#' @details Backwards pivot coordinates map IxJ-part compositional table from the simplex into a (IJ-1)-dimensional real space isometrically. 
#' Particularly the first coordinate from each group (rbpb.1, cbpb.1, tbpc.1) preserves the elemental information on the two-factorial structure. 
#' The first row and column backwards pivot balances rbpb.1 and cbpb.1 represent two-factorial counterparts to the pairwise logratios. 
#' More specifically, the first two levels of the considered factor are compared in the ratio, while the first level plays the role of the rationing category (denominator of the ratio) and the second level is treated as the normalized category (numerator of the ratio). All categories of the complementary factor are aggregated with the geometric mean.
#' The first table backwards pivot coordinate, has form of a four-part log odds-ratio (again related to the first two levels of the row and column factors) and quantifies the relations between factors.
#' All coordinates are structured as detailed in Nesrstova et al. (2023).
#' @keywords multivariate
#' @export
#' @seealso 
#' \code{\link{bpc}} 
#' \code{\link{bpcTabWrapper}} 
#' \code{\link{bpcPcaTab}}
#' \code{\link{bpcRegTab}}
#' @return 
#' \item{Coordinates}{array of orthonormal coordinates.} 
#' \item{Coordinates.ortg}{array of orthogonal coordinates.} 
#' \item{Contrast.matrix}{contrast matrix corresponding to the orthonormal coordinates.}
#' \item{Base}{the base with respect to which logarithms are computed.}
#' \item{Row.levels}{order of the row factor levels.}
#' \item{Col.levels}{order of the column factor levels.}
#' @examples 
#' data(manu_abs)
#' manu_USA <- manu_abs[which(manu_abs$country=='USA'),]
#' manu_USA$output <- as.factor(manu_USA$output)
#' manu_USA$isic <- as.factor(manu_USA$isic)
#' 
#' # default setting with ln()
#' bpcTab(manu_USA, row.factor = "output", col.factor = "isic", value = "value")
#' 
#' # logarithm of base 2
#' bpcTab(manu_USA, row.factor = "output", col.factor = "isic", value = "value",
#' base = 2)
#' 
#' # for base exp(1) is the result similar to tabCoord():
#' r <- rbind(c(-1,1,0), c(-1,-1,1))
#' c <- rbind(c(-1,1,0,0,0), c(-1,-1,1,0,0), c(-1,-1,-1,1,0), c(-1,-1,-1,-1,1))
#' tabCoord(manu_USA, row.factor = "output", col.factor = "isic", value = "value",
#' SBPr = r, SBPc = c)



bpcTab <- function(x,
                   row.factor = NULL,
                   col.factor = NULL,
                   value = NULL,
                   base = exp(1))
{
  I <- nlevels(x[, row.factor])
  J <- nlevels(x[, col.factor])
  
  x.sort <- dplyr::arrange(x, x[, row.factor], x[, col.factor])
  
  # entries of the contrast matrix
  cont.mat.row <- matrix(NA, I-1, I*J)
  for(i in 1:(I-1))
    cont.mat.row[i,] <- norm1(c(rep(-1/i, i*J), rep(1, J), rep(0, (I-i-1)*J))) 
  rownames(cont.mat.row) <- paste("rbpb", c(1:(I-1)), sep = ".")
  
  cont.mat.col <- matrix(NA, J-1, I*J)
  for(j in 1:(J-1))
    cont.mat.col[j,] <- norm1(rep(c(rep(-1/j, j), 1, rep(0, J-j-1)), I))
  rownames(cont.mat.col) <- paste("cbpb", c(1:(J-1)), sep = ".")
  
  cont.mats <- as.list(tidyr::expand_grid(cont.mat.row, cont.mat.col))
  cont.mat.OR <- cont.mats$cont.mat.row * cont.mats$cont.mat.col
  cont.mat.OR <- t(apply(cont.mat.OR, 1, norm1)) 
  rownames(cont.mat.OR) <- paste("tbpc", rep(1:(I-1), each = J-1), 1:(J-1), sep = ".")
  
  # normalizing constants
  norm.const.row <- apply(cont.mat.row, 1, function(x) {r = sum(x > 0); s = sum(x < 0); sqrt(r*s/(r+s))})
  names(norm.const.row) <- paste("rbpb", c(1:(I-1)), sep = ".")
  
  norm.const.col <- apply(cont.mat.col, 1, function(x) {r = sum(x > 0); s = sum(x < 0); sqrt(r*s/(r+s))})
  names(norm.const.col) <- paste("cbpb", c(1:(J-1)), sep = ".")

  norm.consts <- tidyr::expand_grid(norm.const.row, norm.const.col)
  norm.const.OR <- apply(norm.consts, 1, function(x) x[1]*x[2]/sqrt(I*J)) 
  names(norm.const.OR) <- paste("tbpc", rep(1:(I-1), each = J-1), 1:(J-1), sep = ".")
  
  # orthonormal coordinates
  rbpb <- c(cont.mat.row %*% log(x.sort[, value], base = base))
  cbpb <- c(cont.mat.col %*% log(x.sort[, value], base = base))
  tbpc <- c(cont.mat.OR %*% log(x.sort[, value], base = base))
  
  names(rbpb) <- rownames(cont.mat.row)
  names(cbpb) <- rownames(cont.mat.col)
  names(tbpc) <- rownames(cont.mat.OR)
  
  # orthogonal coordinates
  rbpb.og <- rbpb/norm.const.row
  cbpb.og <- cbpb/norm.const.col
  tbpc.og <- tbpc/norm.const.OR
  
  # finalization of results
  contrast.matrix <- rbind(cont.mat.row, cont.mat.col, cont.mat.OR)
  colnames(contrast.matrix) <- paste(x.sort[, row.factor], x.sort[, col.factor], sep = "_")
  
  coordinates <- c(rbpb, cbpb, tbpc)
  coordinates.og <- c(rbpb.og, cbpb.og, tbpc.og)
  
  row.levels <- levels(x[, row.factor])
  col.levels <- levels(x[, col.factor])
  
  message(paste0("The first row backwards pivot balance (rbpb.1) corresponds to the '", row.levels[2], " to ", row.levels[1], "' ratio."))
  message(paste0("The first column backwards pivot balance (cbpb.1) corresponds to the '", col.levels[2], " to ", col.levels[1], "' ratio."))
  message(paste0("The first table backwards pivot coordinate (tbpc.1.1) corresponds to the odds ratio formed by categories '", row.levels[2], ", ", row.levels[1], "' and '", col.levels[2], ", ", col.levels[1], "'."))
  
  return(list("Coordinates" = coordinates, "Coordinates.ortg" = coordinates.og, 
              "Contrast.matrix" = contrast.matrix, "Base" = base, 
              "Row.levels" = row.levels, "Col.levels" = col.levels))
}  

