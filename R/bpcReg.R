#' bpcReg
#' 
#' @title Classical and robust regression based on backwards pivot coordinates
#' @importFrom tidyr expand_grid
#' @importFrom robustbase lmrob
#' @author Kamila Facevicova
#' @references Hron, K., Coenders, G., Filzmoser, P., Palarea-Albaladejo, J., Famera, M., Matys Grygar, M. (2022). Analysing pairwise logratios revisited. Mathematical Geosciences 53, 1643 - 1666.
#' @references Nesrstova, V., Jaskova, P., Pavlu, I., Hron, K., Palarea-Albaladejo, J., Gaba, A., Pelclova, J., Facevicova, K. (2023). Simple enough, but not simpler: Reconsidering additive logratio coordinates in compositional analysis. Submitted
#' @description Performs classical or robust regression analysis of real response on compositional predictors, represented in backwards pivot coordinates. Also non-compositional covariates can be included (additively).
#' 
#' @param X object of class data.frame with compositional (positive values only) and non-compositional predictors. The response y can be also included.
#' @param y character with the name of response (if included in X) or an array with values of the response. 
#' @param external array with names of non-compositional predictors.
#' @param norm.cat the rationing category placed at the first position in the composition. If not defined, all pairwise logratios are considered. Given in quotation marks.
#' @param robust if TRUE, the MM-type estimator is used. Defaults to FALSE.
#' @param base a positive number: the base with respect to which logarithms are computed. Defaults to exp(1).
#' @param norm.const if TRUE, the regression coefficients corresponding to orthonormal coordinates are given a s result. Defaults to FALSE, the normalising constant is omitted.
#' @param seed a single value.
#' 
#' @details The compositional part of the data set is repeatedly expressed in a set of backwards logratio coordinates, when each set highlights one pairwise logratio (or one pairwise logratio with the selected rationing category). 
#' For each set (supplemented by non-compositonal predictors), robust MM or classical least squares estimate of regression coefficients is performed and information respective to the first backwards pivot coordinate is stored. 
#' The summary therefore collects results from several regression models, each leading to the same overall model characteristics, like the F statistics or R^2.
#' The coordinates are structured as detailed in Nesrstova et al. (2023).
#' In order to maintain consistency of the iterative results collected in the output, a seed is set before robust estimation of each of the models considered. Its specific value can be set via parameter seed.
#'
#' @keywords regression
#' @export
#' @seealso 
#' \code{\link{bpc}} 
#' \code{\link{bpcPca}}
#' \code{\link{bpcRegTab}}
#' @return A list containing:\describe{
#' \item{Summary}{the summary object which collects results from all coordinate systems. The names of the coefficients indicate the type of the respective coordinate 
#' (bpc.1 - the first backwards pivot coordinate) and the logratio quantified thereby. 
#' E.g. bpc.1_C2.to.C1 would therefore correspond to the logratio between compositional parts C1 and C2, schematically written log(C2/C1). See Nesrstova et al. (2023) for details.} 
#' \item{Base}{the base with respect to which logarithms are computed} 
#' \item{Norm.const}{the values of normalising constants (when results for orthonormal coordinates are reported).}
#' \item{Robust}{TRUE if the MM estimator was applied.}
#' \item{lm}{the lm object resulting from the first iteration.}
#' \item{Levels}{the order of compositional parts cosidered in the first iteration.}
#' }
#' @examples 
#' ## How the total household expenditures in EU Member
#' ## States depend on relative contributions of 
#' ## single household expenditures:
#' data(expendituresEU)
#' y <- as.numeric(apply(expendituresEU,1,sum))
#' 
#' # classical regression summarizing the effect of all pairwise logratios 
#' lm.cla <- bpcReg(expendituresEU, y)
#' lm.cla
#' 
#' # gives the same model characteristics as lmCoDaX:
#' lm <- lmCoDaX(y, expendituresEU, method="classical")
#' lm$ilr
#' 
#' # robust regression, with Food as the rationing category and logarithm of base 2
#' # response is part of the data matrix X
#' expendituresEU.y <- data.frame(expendituresEU, total = y)
#' lm.rob <- bpcReg(expendituresEU.y, "total", norm.cat = "Food", robust = TRUE, base = 2)
#' lm.rob
#' 
#' ## Illustrative example with exports and imports (categorized) as non-compositional covariates
#' data(economy)
#' X.ext <- economy[!economy$country2 %in% c("HR", "NO", "CH"), c("exports", "imports")]
#' X.ext$imports.cat <- cut(X.ext$imports, quantile(X.ext$imports, c(0, 1/3, 2/3, 1)), 
#' labels = c("A", "B", "C"), include.lowest = TRUE)
#' 
#' X.y.ext <- data.frame(expendituresEU.y, X.ext[, c("exports", "imports.cat")])
#' 
#' lm.ext <- bpcReg(X.y.ext, y = "total", external = c("exports", "imports.cat"))
#' lm.ext

bpcReg <- function(X, y, external = NULL, norm.cat = NULL, robust = FALSE, base = exp(1), norm.const = F, seed = 8)
{
  # preparation of response
  if(is.character(y))
  {
    response <- as.data.frame(X[, y]) 
    colnames(response) <- c("resp") 
    X[, y] <- NULL
  } else
    response <- data.frame(resp = y)
  
  if(!is.null(external))
  {
    X.ext <- X[, external]
    X <- X[, !colnames(X) %in% external]
  } 
  
  D <- ncol(X)
  components <- colnames(X)
  
  # considered orders of categories
  if(is.null(norm.cat)){
    # pairs <- gtools::combinations(D, 2, components)
    pairs <- t(combn(components, 2))
    } else{ 
      pairs <- as.matrix(tidyr::expand_grid(norm.cat, components[components!= norm.cat]), colnames = NULL)
    }
  # initialization
  order.init <- c(pairs[1, ], components[!components %in% pairs[1, ]])
  X.init <- X[, order.init]
  system.init <- suppressMessages(bpc(X.init, base = base))
  
  if(norm.const == TRUE)
    coords.init <- system.init$Coordinates else 
      coords.init <- system.init$Coordinates.ortg
  
  # data matrix used in the regression
  if(!is.null(external))
    d <- data.frame(response, coords.init, X.ext) else 
      d <- data.frame(response, coords.init)

  if(robust == TRUE)
  {
    set.seed(seed)
    lm.init <- robustbase::lmrob(resp ~ ., data = d)
  } else 
      lm.init <- lm(resp ~ ., data = d) 
  
  # preparation of the object with the final results
  lm.sum <- summary(lm.init)
  
  # only the information on the pairwise logratio bpc.1 is collected in the final result
  lm.sum$coefficients <- summary(lm.init)$coefficients[c("(Intercept)", "bpc.1"), ] 
  rownames(lm.sum$coefficients)[2] <- paste("bpc.1_", pairs[1, 2], ".to.", pairs[1, 1], sep = "") 
  
  # coefficents of non-compositional predictors
  lm.init.external <- summary(lm.init)$coefficients[-c(1:D), , drop = F]
  
  # repeated estimation of the regression coefficients related to the remaining pairwise logratios
  for(i in 2:nrow(pairs))
  {
    order.iter <- c(pairs[i, ], components[!components %in% pairs[i, ]])
    X.iter <- X[, order.iter]
    system.iter <- suppressMessages(bpc(X.iter, base = base))
    
    if(norm.const == TRUE)
      coords.iter <- system.iter$Coordinates else 
        coords.iter <- system.iter$Coordinates.ortg
    
    if(!is.null(external))
      d.iter <- data.frame(response, coords.iter, X.ext) else 
        d.iter <- data.frame(response, coords.iter)
    
    if(robust == TRUE)
    {
      set.seed(seed)
      lm.iter <- robustbase::lmrob(resp ~ ., data = d.iter)
    } else 
        lm.iter <- lm(resp ~ ., data = d.iter) 
    
    lm.iter.coeffs <- summary(lm.iter)$coefficients["bpc.1", ] 
    lm.sum$coefficients <- rbind(lm.sum$coefficients, lm.iter.coeffs)
    rownames(lm.sum$coefficients)[i + 1] <- paste("bpc.1_", pairs[i, 2], ".to.", pairs[i, 1], sep = "") 
  }
  
  # the vector of normalising constants (if applicable)
  if(norm.const == TRUE)
  {
    norm.const.values <- rep(sqrt(1/2), nrow(pairs))
    names(norm.const.values) <- rownames(lm.sum$coefficients)[-1]
  } else 
      norm.const.values <- norm.const
  
  if(!is.null(external))
  {
    lm.sum$coefficients <- rbind(lm.sum$coefficients, lm.init.external)
  }

  return(list(Summary = lm.sum, Base = base, Norm.const = norm.const.values, Robust = robust,
              lm = lm.init, Levels = order.init))
}

