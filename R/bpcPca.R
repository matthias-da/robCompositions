#' bpcPca
#' 
#' @title Principal component analysis based on backwards pivot coordinates
#' @importFrom tidyr expand_grid
#' @importFrom robustbase covMcd
#' @author Kamila Facevicova
#' @references Hron, K., Coenders, G., Filzmoser, P., Palarea-Albaladejo, J., Famera, M., Matys Grygar, M. (2022). Analysing pairwise logratios revisited. Mathematical Geosciences 53, 1643 - 1666.
#' @references Nesrstova, V., Jaskova, P., Pavlu, I., Hron, K., Palarea-Albaladejo, J., Gaba, A., Pelclova, J., Facevicova, K. (2023). Simple enough, but not simpler: Reconsidering additive logratio coordinates in compositional analysis. Submitted
#' @description Performs classical or robust principal component analysis on system of backwards pivot coordinates and returns the result related to pairwise logratios as well as the clr representation.
#' 
#' @param X object of class data.frame. Positive values only.
#' @param robust if TRUE, the MCD estimate is used. Defaults to FALSE.
#' @param norm.cat the rationing category placed at the first position in the composition. If not defined, all pairwise logratios are considered. Given in quotation marks.
#'
#' @details The compositional data set is repeatedly expressed in a set of backwards logratio coordinates, when each set highlights one pairwise logratio (or one pairwise logratio with the selected rationing category). 
#' For each set, robust or classical principal component analysis is performed and loadings respective to the first backwards pivot coordinate are stored. 
#' The procedure results in matrix of scores (invariant to the specific coordinate system), clr loading matrix and matrix with loadings respective to pairwise logratios.
#' @keywords multivariate 
#' @export
#' @seealso 
#' \code{\link{bpc}} 
#' \code{\link{bpcPcaTab}}
#' \code{\link{bpcReg}}
#' @return 
#' \item{scores}{array of scores.} 
#' \item{loadings}{loadings related to the pairwise logratios. The names of the rows indicate the type of the respective coordinate 
#' (bpc.1 - the first backwards pivot coordinate) and the logratio quantified thereby. 
#' E.g. bpc.1_C2.to.C1 would therefore correspond to the logratio between compositional parts C1 and C2, schematically written log(C2/C1). See Nesrstova et al. (2023) for details.} 
#' \item{loadings.clr}{loadings in the clr space.} 
#' \item{sdev}{standard deviations of the principal components.}
#' \item{center}{means of the pairwise logratios.}
#' \item{center.clr}{means of the clr coordinates.}
#' \item{n.obs}{number of observations.}
#' @examples 
#' data(arcticLake)
#' 
#' # classical estimation with all pairwise logratios:
#' res.cla <- bpcPca(arcticLake)
#' summary(res.cla)
#' biplot(res.cla)
#' head(res.cla$scores)
#' res.cla$loadings
#' res.cla$loadings.clr
#' 
#' # similar output as from pca CoDa
#' res.cla2 <- pcaCoDa(arcticLake, method="classical", solve = "eigen")
#' biplot(res.cla2)
#' head(res.cla2$scores)
#' res.cla2$loadings
#' 
#' # classical estimation focusing on pairwise logratios with clay:
#' res.cla.clay <- bpcPca(arcticLake, norm.cat = "clay")
#' biplot(res.cla.clay)
#' 
#' # robust estimation with all pairwise logratios:
#' res.rob <- bpcPca(arcticLake, robust = TRUE)
#' biplot(res.rob)
#' 
 
bpcPca <- function(X, robust = FALSE, norm.cat = NULL)
{
  D <- ncol(X)
  components <- colnames(X)
  
  # considered pairs of categories
  if(is.null(norm.cat))
    # pairs <- gtools::combinations(D, 2, components) else 
      pairs <- t(combn(components, 2)) else
      pairs <- as.matrix(tidyr::expand_grid(norm.cat, components[components!= norm.cat]), colnames = NULL)
  
  # initialization
  order.init <- c(pairs[1, ], components[!components %in% pairs[1, ]])
  X.init <- X[, order.init]
  coords.init <- suppressMessages(bpc(X.init))$Coordinates
  cont.mat.init <- suppressMessages(bpc(X.init))$Contrast.matrix
  
  if (robust == TRUE) {
    cv <- robustbase::covMcd(coords.init, cor = FALSE)
    pca.Ilr.init <- suppressWarnings(princomp(coords.init, covmat = cv, cor = FALSE))
  } else {
    pca.Ilr.init <- princomp(coords.init, cor = FALSE)
  }
  
  # preparation of the object with the final results
  pca.res <- pca.Ilr.init
  
  # only the information on the pairwise logratio bpc.1 is collected in the final result
  pca.res$center <- pca.res$center["bpc.1"]
  names(pca.res$center)[1] <- c(paste(names(pca.res$center)[1], "_", pairs[1, 2], ".to.", pairs[1, 1], sep = ""))
  pca.res$loadings <- pca.res$loadings["bpc.1",]
  
  # clr loading matrix
  pca.res$loadings.clr <- t(cont.mat.init) %*% pca.Ilr.init$loadings
  
  # clr mean vector
  pca.res$center.clr <- t(cont.mat.init) %*% pca.Ilr.init$center
  
  # computation of the means and loadings related to the remaining pairwise logratios
  for(i in 2:nrow(pairs))
  {
    new.loadings <- sqrt(1/2)*(pca.res$loadings.clr[pairs[i,2], ] - pca.res$loadings.clr[pairs[i,1], ])
    pca.res$loadings <- rbind(pca.res$loadings, new.loadings)
    rownames(pca.res$loadings)[i] <-  c(paste("bpc.1_", pairs[i, 2], ".to.", pairs[i, 1], sep = ""))
    
    new.center <- sqrt(1/2)*(pca.res$center.clr[pairs[i,2], ] - pca.res$center.clr[pairs[i,1], ])
    pca.res$center <- c(pca.res$center, new.center)
    names(pca.res$center)[i] <- c(paste("bpc.1_", pairs[i, 2], ".to.", pairs[i, 1], sep = ""))
  } 
  
  rownames(pca.res$loadings)[1] <- c(paste("bpc.1_", pairs[1, 2], ".to.", pairs[1, 1], sep = ""))
  
  pca.res$scale <- rep(1, length(pca.res$center))
  names(pca.res$scale) <- names(pca.res$center)

  return(pca.res)
}  

