#' bpcPcaTab
#' 
#' @title Principal component analysis of compositional tables based on backwards pivot coordinates
#' @importFrom tidyr expand_grid
#' @author Kamila Facevicova
#' @references Nesrstova, V., Jaskova, P., Pavlu, I., Hron, K., Palarea-Albaladejo, J., Gaba, A., Pelclova, J., Facevicova, K. (2023). Simple enough, but not simpler: Reconsidering additive logratio coordinates in compositional analysis. Submitted
#' @description  Performs classical or robust principal component analysis on a set of compositional tables, based on backwards pivot coordinates. Returns the result related to pairwise row and column balances and four-part log odds-ratios. The loadings in the clr space are available as well.
#' 
#' @param X object of class data.frame with columns corresponding to row and column factors of the respective compositional table, a variable with the values of the composition (positive values only) and a factor with observation IDs.
#' @param obs.ID name of the factor variable distinguishing the observations. Needs to be given with the quotation marks.
#' @param row.factor name of the variable representing the row factor. Needs to be given with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be given with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be given with the quotation marks.
#' @param robust if TRUE, the MCD estimate is used. Defaults to FALSE.
#' @param norm.cat.row the rationing category of the row factor. If not defined, all pairs are considered. Given in quotation marks.
#' @param norm.cat.col the rationing category of the column factor. If not defined, all pairs are considered. Given in quotation marks.
#'
#' @details The set of compositional tables is repeatedly expressed in a set of backwards logratio coordinates, 
#' when each set highlights different combination of pairs of row and column factor categories, as detailed in Nesrstova et al. (2023). 
#' For each set, robust or classical principal component analysis is performed and loadings respective to the first row, column and odds-ratio backwards pivot coordinates are stored. 
#' The procedure results in matrix of scores (invariant to the specific coordinate system), clr loading matrix and matrix with loadings related to the selected backwards coordinates.
#' @keywords multivariate 
#' @export
#' @seealso 
#' \code{\link{bpcTabWrapper}} 
#' \code{\link{bpcPca}}
#' \code{\link{bpcRegTab}}
#' @return 
#' \item{scores}{array of scores.} 
#' \item{loadings}{loadings related to the selected backwards coordinates. The names of the rows indicate the type of the respective coordinate 
#' (rbpb.1 - the first row backwards pivot balance, cbpb.1 - the first column backwards pivot balance and tbpc.1.1 - the first table backwards pivot coordinate) and the logratio or log odds-ratio quantified thereby. 
#' E.g. cbpb.1_C2.to.C1 would therefore correspond to the logratio between column categories C1 and C2, schematically written log(C2/C1), and tbpc.1.1_R2.to.R1.&.C2.to.C1 would correspond to the log odds-ratio computed from a 2x2 table, 
#' which is formed by row categories R1 and R2 and columns C1 and C2. See Nesrstova et al. (2023) for details.} 
#' \item{loadings.clr}{loadings in the clr space. The names of the rows indicate the position of respective part in the clr representation of the compositional table, labeled as row.category_column.category.} 
#' \item{sdev}{standard deviations of the principal components.}
#' \item{center}{means of the selected backwards coordinates.}
#' \item{center.clr}{means of the clr coordinates.}
#' \item{n.obs}{number of observations.}
#'
#' @examples
#' data(manu_abs)
#' manu_abs$output <- as.factor(manu_abs$output)
#' manu_abs$isic <- as.factor(manu_abs$isic)
#' 
#' # classical estimation with all pairwise balances and four-part ORs:
#' res.cla <- bpcPcaTab(manu_abs, obs.ID = "country", row.factor = "output", 
#' col.factor = "isic", value = "value")
#' summary(res.cla)
#' biplot(res.cla)
#' head(res.cla$scores)
#' res.cla$loadings
#' res.cla$loadings.clr
#' 
#' # classical estimation with LAB anf 155 as rationing categories
#' res.cla.select <- bpcPcaTab(manu_abs, obs.ID = "country", row.factor = "output", 
#' col.factor = "isic", value = "value", norm.cat.row = "LAB", norm.cat.col = "155")
#' summary(res.cla.select)
#' biplot(res.cla.select)
#' head(res.cla.select$scores)
#' res.cla.select$loadings
#' res.cla.select$loadings.clr
#' 
#' # robust estimation with all pairwise balances and four-part ORs:
#' res.rob <- bpcPcaTab(manu_abs, obs.ID = "country", row.factor = "output", 
#' col.factor = "isic", value = "value", robust = TRUE)
#' summary(res.rob)
#' biplot(res.rob)
#' head(res.rob$scores)
#' res.rob$loadings
#' res.rob$loadings.clr

bpcPcaTab <- function(X, obs.ID = NULL, row.factor = NULL, col.factor = NULL, 
                      value = NULL, robust = FALSE, norm.cat.row = NULL, 
                      norm.cat.col = NULL)
{
  I <- nlevels(X[, row.factor])
  J <- nlevels(X[, col.factor])
  
  # considered pairs of row and columns categories
  if (is.null(norm.cat.row) & is.null(norm.cat.col)) 
  {
    #row.factor.pairs <- gtools::combinations(nlevels(X[, row.factor]), 2, levels(X[, row.factor]))
    row.factor.pairs <- t(combn(levels(X[, row.factor]), 2))
    #col.factor.pairs <- gtools::combinations(nlevels(X[, col.factor]), 2, levels(X[, col.factor]))
    col.factor.pairs <- t(combn(levels(X[, col.factor]), 2))   
    all.combinations <- as.list(tidyr::expand_grid(row.factor.pairs, col.factor.pairs))
  } else
  {
    if(!is.null(norm.cat.row) & is.null(norm.cat.col))
    {
      row.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.row, levels(X[, row.factor])[levels(X[, row.factor])!= norm.cat.row]), colnames = NULL)
      colnames(row.factor.pairs) <- NULL
      # col.factor.pairs <- gtools::combinations(nlevels(X[, col.factor]), 2, levels(X[, col.factor]))
      col.factor.pairs <- t(combn(levels(X[, col.factor]), 2))   
      all.combinations <- as.list(tidyr::expand_grid(row.factor.pairs, col.factor.pairs))
    } else
    {
      if(is.null(norm.cat.row) & !is.null(norm.cat.col))
      {
        # row.factor.pairs <- gtools::combinations(nlevels(X[, row.factor]), 2, levels(X[, row.factor]))
        row.factor.pairs <- t(combn(levels(X[, row.factor]), 2))   
        col.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.col, levels(X[, col.factor])[levels(X[, col.factor])!= norm.cat.col]), colnames = NULL)
        colnames(col.factor.pairs) <- NULL
        all.combinations <- as.list(tidyr::expand_grid(row.factor.pairs, col.factor.pairs))
      } else
      {
        row.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.row, levels(X[, row.factor])[levels(X[, row.factor])!= norm.cat.row]), colnames = NULL)
        colnames(row.factor.pairs) <- NULL
        col.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.col, levels(X[, col.factor])[levels(X[, col.factor])!= norm.cat.col]), colnames = NULL)
        colnames(col.factor.pairs) <- NULL
        all.combinations <- as.list(tidyr::expand_grid(row.factor.pairs, col.factor.pairs))
      }
    }
    
  }
  
  # initialization
  all.combinations.init <- NULL
  all.combinations.init[[1]] <- c(all.combinations[[1]][1,], levels(X[, row.factor])[!levels(X[, row.factor]) %in% all.combinations[[1]][1,]])
  all.combinations.init[[2]] <- c(all.combinations[[2]][1,], levels(X[, col.factor])[!levels(X[, col.factor]) %in% all.combinations[[2]][1,]])
  
  X[, row.factor] <- factor(X[, row.factor], levels = all.combinations.init[[1]])
  X[, col.factor] <- factor(X[, col.factor], levels = all.combinations.init[[2]])
  
  coords.init <- suppressMessages(bpcTabWrapper(X, obs.ID = obs.ID, row.factor = row.factor, col.factor = col.factor, value = value))$Coordinates
  cont.mat.init <- suppressMessages(bpcTabWrapper(X, obs.ID = obs.ID, row.factor = row.factor, col.factor = col.factor, value = value))$Contrast.matrix

  if (robust == TRUE) {
    cv <- robustbase::covMcd(coords.init, cor = FALSE)
    pca.Ilr.init <- suppressWarnings(princomp(coords.init, covmat = cv, 
                                              cor = FALSE))
  } else {
    pca.Ilr.init <- princomp(coords.init, cor = FALSE)
  }
  
  # preparation of the object with the final results
  pca.res <- pca.Ilr.init
  
  # only the information on the row and column pairwise balances rbpc.1 and cbpb.1 and four-part log OR tbpc.1.1 is collected in the final result
  pca.res$center <- pca.res$center[c("rbpb.1", "cbpb.1", "tbpc.1.1")]
  names(pca.res$center) <- c(paste(names(pca.res$center)[1], "_", all.combinations[[1]][1, 2], ".to.", all.combinations[[1]][1, 1], sep = ""), # oddeluju pomoci "_" abych to podle nej pak mohla snadno odseknout pri tvorbe popisku do biplotu (treba)
                             paste(names(pca.res$center)[2], "_", all.combinations[[2]][1, 2], ".to.", all.combinations[[2]][1, 1], sep = ""),
                             paste(names(pca.res$center)[3], "_", all.combinations[[1]][1, 2], ".to.", all.combinations[[1]][1, 1],".&.", all.combinations[[2]][1, 2], ".to.", all.combinations[[2]][1, 1], sep = ""))
  
  pca.res$loadings <- pca.res$loadings[c("rbpb.1", "cbpb.1", "tbpc.1.1"),]
  rownames(pca.res$loadings)[1] <- paste(rownames(pca.res$loadings)[1], "_", all.combinations[[1]][1, 2], ".to.", all.combinations[[1]][1, 1], sep = "") # oddeluju pomoci "_" abych to podle nej pak mohla snadno odseknout pri tvorbe popisku do biplotu (treba)
  rownames(pca.res$loadings)[2] <- paste(rownames(pca.res$loadings)[2], "_", all.combinations[[2]][1, 2], ".to.", all.combinations[[2]][1, 1], sep = "")
  rownames(pca.res$loadings)[3] <- paste(rownames(pca.res$loadings)[3], "_", all.combinations[[1]][1, 2], ".to.", all.combinations[[1]][1, 1],".&.", all.combinations[[2]][1, 2], ".to.", all.combinations[[2]][1, 1], sep = "")
  
  # clr loading matrix
  pca.res$loadings.clr <- t(cont.mat.init) %*% pca.Ilr.init$loadings
  
  # clr mean vector
  pca.res$center.clr <- t(cont.mat.init) %*% pca.Ilr.init$center
  
  # computation of the centers and loadings related to the remaining coordinates of the interest
  for(i in 2:nrow(all.combinations[[1]]))
  {
    row.balance <- sqrt(1/(2*J))*(colSums(pca.res$loadings.clr[grepl(all.combinations$row.factor.pairs[i,2], rownames(pca.res$loadings.clr)), ]) -
                                    colSums(pca.res$loadings.clr[grepl(all.combinations$row.factor.pairs[i,1], rownames(pca.res$loadings.clr)), ]))
    col.balance <- sqrt(1/(2*I))*(colSums(pca.res$loadings.clr[grepl(all.combinations$col.factor.pairs[i,2], rownames(pca.res$loadings.clr)), ]) -
                                    colSums(pca.res$loadings.clr[grepl(all.combinations$col.factor.pairs[i,1], rownames(pca.res$loadings.clr)), ]))
    OR.coord <- 1/2*(pca.res$loadings.clr[grepl(paste(all.combinations$row.factor.pairs[i,1], all.combinations$col.factor.pairs[i,1], sep = "_"), rownames(pca.res$loadings.clr)), ] +
                       pca.res$loadings.clr[grepl(paste(all.combinations$row.factor.pairs[i,2], all.combinations$col.factor.pairs[i,2], sep = "_"), rownames(pca.res$loadings.clr)), ] -
                       pca.res$loadings.clr[grepl(paste(all.combinations$row.factor.pairs[i,1], all.combinations$col.factor.pairs[i,2], sep = "_"), rownames(pca.res$loadings.clr)), ] -
                       pca.res$loadings.clr[grepl(paste(all.combinations$row.factor.pairs[i,2], all.combinations$col.factor.pairs[i,1], sep = "_"), rownames(pca.res$loadings.clr)), ])
    new.loadings <- rbind(row.balance, col.balance, OR.coord) 
    rownames(new.loadings) <-  c(paste("rbpb.1_", all.combinations[[1]][i, 2], ".to.", all.combinations[[1]][i, 1], sep = ""),
                                 paste("cbpb.1_", all.combinations[[2]][i, 2], ".to.", all.combinations[[2]][i, 1], sep = ""),
                                 paste("tbpc.1.1_", all.combinations[[1]][i, 2], ".to.", all.combinations[[1]][i, 1],".&.", all.combinations[[2]][i, 2], ".to.", all.combinations[[2]][i, 1], sep = ""))
    pca.res$loadings <- rbind(pca.res$loadings, new.loadings)
    
    row.balance.center <- sqrt(1/(2*J))*(sum(pca.res$center.clr[grepl(all.combinations$row.factor.pairs[i,2], rownames(pca.res$center.clr)), ]) -
                                           sum(pca.res$center.clr[grepl(all.combinations$row.factor.pairs[i,1], rownames(pca.res$center.clr)), ]))
    col.balance.center <- sqrt(1/(2*I))*(sum(pca.res$center.clr[grepl(all.combinations$col.factor.pairs[i,2], rownames(pca.res$center.clr)), ]) -
                                           sum(pca.res$center.clr[grepl(all.combinations$col.factor.pairs[i,1], rownames(pca.res$center.clr)), ]))
    OR.coord.center <- 1/2*(pca.res$center.clr[grepl(paste(all.combinations$row.factor.pairs[i,1], all.combinations$col.factor.pairs[i,1], sep = "_"), rownames(pca.res$center.clr)), ] +
                              pca.res$center.clr[grepl(paste(all.combinations$row.factor.pairs[i,2], all.combinations$col.factor.pairs[i,2], sep = "_"), rownames(pca.res$center.clr)), ] -
                              pca.res$center.clr[grepl(paste(all.combinations$row.factor.pairs[i,1], all.combinations$col.factor.pairs[i,2], sep = "_"), rownames(pca.res$center.clr)), ] -
                              pca.res$center.clr[grepl(paste(all.combinations$row.factor.pairs[i,2], all.combinations$col.factor.pairs[i,1], sep = "_"), rownames(pca.res$center.clr)), ])
    new.centers <- c(row.balance.center, col.balance.center, OR.coord.center) 
    names(new.centers) <-  c(paste("rbpb.1_", all.combinations[[1]][i, 2], ".to.", all.combinations[[1]][i, 1], sep = ""), 
                             paste("cbpb.1_", all.combinations[[2]][i, 2], ".to.", all.combinations[[2]][i, 1], sep = ""),
                             paste("tbpc.1.1_", all.combinations[[1]][i, 2], ".to.", all.combinations[[1]][i, 1],".&.", all.combinations[[2]][i, 2], ".to.", all.combinations[[2]][i, 1], sep = ""))
    pca.res$center <- c(pca.res$center, new.centers)
  }
  
  # remove the overlapping information
  pca.res$loadings <- pca.res$loadings[unique(rownames(pca.res$loadings)),]  
  pca.res$center <- pca.res$center[unique(names(pca.res$center))]
  
  pca.res$loadings <- pca.res$loadings[order(rownames(pca.res$loadings)),]
  pca.res$center <- pca.res$center[order(names(pca.res$center))]
  
  pca.res$scale <- rep(1, length(pca.res$center))
  names(pca.res$scale) <- names(pca.res$center)
  
  return(pca.res)
}
