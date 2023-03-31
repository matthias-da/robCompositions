#' bpcRegTab
#' 
#' @title Classical and robust regression based on backwards pivot coordinates
#' @importFrom tidyr expand_grid
#' @importFrom dplyr distinct
#' @author Kamila Facevicova
#' @references Nesrstova, V., Jaskova, P., Pavlu, I., Hron, K., Palarea-Albaladejo, J., Gaba, A., Pelclova, J., Facevicova, K. (2023). Simple enough, but not simpler: Reconsidering additive logratio coordinates in compositional analysis. Submitted
#' @description  Performs classical or robust regression analysis of real response on a compositional table, which is represented in backwards pivot coordinates.  Also non-compositional covariates can be included (additively).
#' 
#' @param X object of class data.frame with columns corresponding to row and column factors of the respective compositional table, a variable with the values of the composition (positive values only) and a factor with observation IDs. The response y and non-compositional predictors can be also included.
#' @param y character with the name of response (if included in X), data frame with row names corresponding to observation IDs or a named array with values of the response. 
#' @param obs.ID name of the factor variable distinguishing the observations. Needs to be given with the quotation marks.
#' @param row.factor name of the variable representing the row factor. Needs to be given with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be given with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be given with the quotation marks.
#' @param external array with names of non-compositional predictors.
#' @param norm.cat.row the rationing category of the row factor. If not defined, all pairs are considered. Given in quotation marks.
#' @param norm.cat.col the rationing category of the column factor. If not defined, all pairs are considered. Given in quotation marks.
#' @param robust if TRUE, the MM-type estimator is used. Defaults to FALSE.
#' @param base a positive number: the base with respect to which logarithms are computed. Defaults to exp(1).
#' @param norm.const if TRUE, the regression coefficients corresponding to orthonormal coordinates are given a s result. Defaults to FALSE, the normalising constant is omitted.
#' @param seed a single value.
#' 
#' @details The set of compositional tables is repeatedly expressed in a set of backwards logratio coordinates, 
#' when each set highlights different combination of pairs of row and column factor categories, as detailed in Nesrstova et al. (2023).
#' For each coordinates system (supplemented by non-compositonal predictors), robust MM or classical least squares estimate of regression coefficients is performed and information respective to the first row, column and table backwards pivot coordinate is stored. 
#' The summary therefore collects results from several regression models, each leading to the same overall model characteristics, like the F statistics or R^2. 
#' In order to maintain consistency of the iterative results collected in the output, a seed is set before robust estimation of each of the models considered. Its specific value can be set via parameter seed.
#' 
#' @keywords regression
#' @export
#' @seealso 
#' \code{\link{bpcTabWrapper}} 
#' \code{\link{bpcPcaTab}}
#' \code{\link{bpcReg}}
#' @return A list containing:\describe{
#' \item{Summary}{the summary object which collects results from all coordinate systems. The names of the coefficients indicate the type of the respective coordinate 
#' (rbpb.1 - the first row backwards pivot balance, cbpb.1 - the first column backwards pivot balance and tbpc.1.1 - the first table backwards pivot coordinate) and the logratio or log odds-ratio quantified thereby. 
#' E.g. cbpb.1_C2.to.C1 would therefore correspond to the logratio between column categories C1 and C2, schematically written log(C2/C1), and tbpc.1.1_R2.to.R1.&.C2.to.C1 would correspond to the log odds-ratio computed from a 2x2 table, 
#' which is formed by row categories R1 and R2 and columns C1 and C2. See Nesrstova et al. (2023) for details.} 
#' \item{Base}{the base with respect to which logarithms are computed} 
#' \item{Norm.const}{the values of normalising constants (when results for orthonormal coordinates are reported).}
#' \item{Robust}{TRUE if the MM estimator was applied.}
#' \item{lm}{the lm object resulting from the first iteration.}
#' \item{Row.levels}{the order of the row factor levels cosidered in the first iteration.}
#' \item{Col.levels}{the order of the column factor levels cosidered in the first iteration.}
#' }
#'
#' @examples
#' # let's prepare some data
#' data(employment2)
#' data(unemployed)
#' 
#' table_data <- employment2[employment2$Contract == "FT", ]
#' y <- unemployed[unemployed$age == "20_24" & unemployed$year == 2015,]
#' countries <- intersect(levels(droplevels(y$country)), levels(table_data$Country))
#' 
#' table_data <- table_data[table_data$Country %in% countries, ]
#' y <- y[y$country %in% countries, c("country", "value")]
#' colnames(y) <- c("Country", "unemployed")
#' 
#' # response as part of X
#' table_data.y <- merge(table_data, y, by = "Country")
#' reg.cla <- bpcRegTab(table_data.y, y = "unemployed", obs.ID = "Country", 
#' row.factor = "Sex", col.factor = "Age", value = "Value")
#' reg.cla
#' 
#' # response as named array
#' resp <- y$unemployed
#' names(resp) <- y$Country
#' reg.cla2 <- bpcRegTab(table_data.y, y = resp, obs.ID = "Country", 
#' row.factor = "Sex", col.factor = "Age", value = "Value")
#' reg.cla2
#' 
#' # response as data.frame, robust estimator, 55plus as the rationing category, logarithm of base 2
#' resp.df <- as.data.frame(y$unemployed)
#' rownames(resp.df) <- y$Country
#' reg.rob <- bpcRegTab(table_data.y, y = resp.df, obs.ID = "Country", 
#' row.factor = "Sex", col.factor = "Age", value = "Value",
#' norm.cat.col = "55plus", robust = TRUE, base = 2)
#' reg.rob
#' 
#' # Illustrative example with non-compositional predictors and response as part of X
#' x.ext <- unemployed[unemployed$age == "15_19" & unemployed$year == 2015,]
#' x.ext <- x.ext[x.ext$country %in% countries, c("country", "value")]
#' colnames(x.ext) <- c("Country", "15_19")
#' 
#' table_data.y.ext <- merge(table_data.y, x.ext, by = "Country")
#' reg.cla.ext <- bpcRegTab(table_data.y.ext, y = "unemployed", obs.ID = "Country", 
#' row.factor = "Sex", col.factor = "Age", value = "Value", external = "15_19")
#' reg.cla.ext


bpcRegTab <- function(X, y, obs.ID = NULL, row.factor = NULL, col.factor = NULL, 
                      value = NULL, external = NULL,
                      norm.cat.row = NULL, norm.cat.col = NULL, 
                      robust = FALSE, base = exp(1), norm.const = F, seed = 8)
{
  # preparation of response
  if(is.character(y))
  {
    response <- X[, c(obs.ID, y)] 
    response <- dplyr::distinct(response, .keep_all = TRUE) 
    rownames(response) <- response[, obs.ID]
    response[, obs.ID] <- NULL
    colnames(response) <- c("resp")
  } else
  {
    if(is.data.frame(y))
    {
      response <- y
      colnames(response) <- c("resp")
    } else
      response <- data.frame(resp = y)
  }

  if(!is.null(external))
  {
    X.ext <- X[, c(obs.ID, external)]
    X.ext <- dplyr::distinct(X.ext, .keep_all = TRUE) 
    rownames(X.ext) <- X.ext[, obs.ID]
    X.ext[, obs.ID] <- NULL
    
    X <- X[, !colnames(X) %in% external]
  } 
    
  I <- nlevels(X[, row.factor])
  J <- nlevels(X[, col.factor])
  
  # considered orders of categories
  if (is.null(norm.cat.row) & is.null(norm.cat.col))
  {
    # row.factor.pairs <- gtools::combinations(nlevels(X[, row.factor]), 2, levels(X[, row.factor]))
    row.factor.pairs <- t(combn(levels(X[, row.factor]), 2))
    row.factor.rest <- apply(row.factor.pairs, 1, function(x)levels(X[, row.factor])[!levels(X[, row.factor]) %in% x])
    row.factor.levels <- cbind(row.factor.pairs, row.factor.rest)
    colnames(row.factor.levels) <- NULL
    
    # col.factor.pairs <- gtools::combinations(nlevels(X[, col.factor]), 2, levels(X[, col.factor]))
    col.factor.pairs <- t(combn(levels(X[, col.factor]), 2))   
    col.factor.rest <- apply(col.factor.pairs, 1, function(x)levels(X[, col.factor])[!levels(X[, col.factor]) %in% x])
    col.factor.levels <- cbind(col.factor.pairs, col.factor.rest)
    colnames(col.factor.levels) <- NULL
    
    all.combinations <- as.list(tidyr::expand_grid(row.factor.levels, col.factor.levels))
    
  } else
  {
    if(!is.null(norm.cat.row) & is.null(norm.cat.col))
    {
      row.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.row, levels(X[, row.factor])[levels(X[, row.factor])!= norm.cat.row]), colnames = NULL)
      row.factor.rest <- apply(row.factor.pairs, 1, function(x)levels(X[, row.factor])[!levels(X[, row.factor]) %in% x])
      row.factor.levels <- cbind(row.factor.pairs, row.factor.rest)
      colnames(row.factor.levels) <- NULL
      
      #col.factor.pairs <- gtools::combinations(nlevels(X[, col.factor]), 2, levels(X[, col.factor]))
      col.factor.pairs <- t(combn(levels(X[, col.factor]), 2))
      col.factor.rest <- apply(col.factor.pairs, 1, function(x)levels(X[, col.factor])[!levels(X[, col.factor]) %in% x])
      col.factor.levels <- cbind(col.factor.pairs, col.factor.rest)
      colnames(col.factor.levels) <- NULL
      
      all.combinations <- as.list(tidyr::expand_grid(row.factor.levels, col.factor.levels))
      
    } else
    {
      if(is.null(norm.cat.row) & !is.null(norm.cat.col))
      {
        #row.factor.pairs <- gtools::combinations(nlevels(X[, row.factor]), 2, levels(X[, row.factor]))
        row.factor.pairs <- t(combn(levels(X[, row.factor]), 2))  
        row.factor.rest <- apply(row.factor.pairs, 1, function(x)levels(X[, row.factor])[!levels(X[, row.factor]) %in% x])
        row.factor.levels <- cbind(row.factor.pairs, row.factor.rest)
        colnames(row.factor.levels) <- NULL
        
        col.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.col, levels(X[, col.factor])[levels(X[, col.factor])!= norm.cat.col]), colnames = NULL)
        col.factor.rest <- apply(col.factor.pairs, 1, function(x)levels(X[, col.factor])[!levels(X[, col.factor]) %in% x])
        col.factor.levels <- cbind(col.factor.pairs, col.factor.rest)
        colnames(col.factor.levels) <- NULL
        
        all.combinations <- as.list(tidyr::expand_grid(row.factor.levels, col.factor.levels))
        
      } else
      {
        row.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.row, levels(X[, row.factor])[levels(X[, row.factor])!= norm.cat.row]), colnames = NULL)
        row.factor.rest <- apply(row.factor.pairs, 1, function(x)levels(X[, row.factor])[!levels(X[, row.factor]) %in% x])
        row.factor.levels <- cbind(row.factor.pairs, row.factor.rest)
        colnames(row.factor.levels) <- NULL
        
        col.factor.pairs <- as.matrix(tidyr::expand_grid(norm.cat.col, levels(X[, col.factor])[levels(X[, col.factor])!= norm.cat.col]), colnames = NULL)
        col.factor.rest <- apply(col.factor.pairs, 1, function(x)levels(X[, col.factor])[!levels(X[, col.factor]) %in% x])
        col.factor.levels <- cbind(col.factor.pairs, col.factor.rest)
        colnames(col.factor.levels) <- NULL
        
        all.combinations <- as.list(tidyr::expand_grid(row.factor.levels, col.factor.levels))
      }
    }
    
  }
  
  # initialization  
  X[, row.factor] <- factor(X[, row.factor], levels = all.combinations[[1]][1, ])
  X[, col.factor] <- factor(X[, col.factor], levels = all.combinations[[2]][1, ])
  
  system.init <- suppressMessages(bpcTabWrapper(X, obs.ID = obs.ID, row.factor = row.factor, col.factor = col.factor, value = value, base = base))
  
  if(norm.const == TRUE)
    coords.init <- system.init$Coordinates else 
      coords.init <- system.init$Coordinates.ortg

  # data matrix used in the regression
  if(!is.null(external))
  {
    cov.init <- merge(coords.init, X.ext, by = "row.names") 
    rownames(cov.init) <- cov.init$Row.names
    cov.init$Row.names <- NULL 
    
    d <- merge(response, cov.init, by = "row.names") 
    rownames(d) <- d$Row.names
    d$Row.names <- NULL 
  } else 
  {
    d <- merge(response, coords.init, by = "row.names") 
    rownames(d) <- d$Row.names
    d$Row.names <- NULL 
  }
  
  if(robust == TRUE)
  {
    set.seed(seed)
    lm.init <- robustbase::lmrob(resp ~ ., data = d)
  } else 
      lm.init <- lm(resp ~ ., data = d) 
  
  # preparation of the object with the final results
  lm.sum <- summary(lm.init) 
  
  # only the information on the first row, column and table coordinates is collected in the final result
  lm.sum$coefficients <- summary(lm.init)$coefficients[c("(Intercept)", "rbpb.1", "cbpb.1", "tbpc.1.1"), ] 
  rownames(lm.sum$coefficients)[2] <- paste(rownames(lm.sum$coefficients)[2], "_", all.combinations[[1]][1, 2], ".to.", all.combinations[[1]][1, 1], sep = "") # oddeluju pomoci "_" abych to podle nej pak mohla snadno odseknout pri tvorbe popisku do biplotu (treba)
  rownames(lm.sum$coefficients)[3] <- paste(rownames(lm.sum$coefficients)[3], "_", all.combinations[[2]][1, 2], ".to.", all.combinations[[2]][1, 1], sep = "")
  rownames(lm.sum$coefficients)[4] <- paste(rownames(lm.sum$coefficients)[4], "_", all.combinations[[1]][1, 2], ".to.", all.combinations[[1]][1, 1],".&.", all.combinations[[2]][1, 2], ".to.", all.combinations[[2]][1, 1], sep = "")

  # coefficents of non-compositional predictors
  lm.init.external <- summary(lm.init)$coefficients[-c(1:(I*J)), , drop = F]
  
  # repeated estimation of the regression coefficients related to the remaining sources of elemental information
  for(i in 2:nrow(all.combinations[[1]]))
  {
    X[, row.factor] <- factor(X[, row.factor], levels = all.combinations[[1]][i,])
    X[, col.factor] <- factor(X[, col.factor], levels = all.combinations[[2]][i,])
    
    system.iter <- suppressMessages(bpcTabWrapper(X, obs.ID = obs.ID, row.factor = row.factor, col.factor = col.factor, value = value, base = base))
    
    if(norm.const == TRUE)
      coords.iter <- system.iter$Coordinates else 
        coords.iter <- system.iter$Coordinates.ortg
    
    if(!is.null(external))
    {
      cov.iter <- merge(coords.iter, X.ext, by = "row.names") 
      rownames(cov.iter) <- cov.iter$Row.names
      cov.iter$Row.names <- NULL 
      
      d.iter <- merge(response, cov.iter, by = "row.names") 
      rownames(d.iter) <- d.iter$Row.names
      d.iter$Row.names <- NULL 
    } else 
    {
      d.iter <- merge(response, coords.iter, by = "row.names")
      rownames(d.iter) <- d.iter$Row.names
      d.iter$Row.names <- NULL 
    }
    
    if(robust == TRUE)
    {
      set.seed(seed)
      lm.iter <- robustbase::lmrob(resp ~ ., data = d.iter)
    } else 
        lm.iter <- lm(resp ~ ., data = d.iter) 
    
    lm.iter.coeffs <- summary(lm.iter)$coefficients[c("rbpb.1", "cbpb.1", "tbpc.1.1"), ] 
    
    rownames(lm.iter.coeffs)[1] <- paste(rownames(lm.iter.coeffs)[1], "_", all.combinations[[1]][i, 2], ".to.", all.combinations[[1]][i, 1], sep = "") # oddeluju pomoci "_" abych to podle nej pak mohla snadno odseknout pri tvorbe popisku do biplotu (treba)
    rownames(lm.iter.coeffs)[2] <- paste(rownames(lm.iter.coeffs)[2], "_", all.combinations[[2]][i, 2], ".to.", all.combinations[[2]][i, 1], sep = "")
    rownames(lm.iter.coeffs)[3] <- paste(rownames(lm.iter.coeffs)[3], "_", all.combinations[[1]][i, 2], ".to.", all.combinations[[1]][i, 1],".&.", all.combinations[[2]][i, 2], ".to.", all.combinations[[2]][i, 1], sep = "")
    
    lm.sum$coefficients <- rbind(lm.sum$coefficients, lm.iter.coeffs)
  }
  
  # remove the overlapping information  
  lm.sum$coefficients <- lm.sum$coefficients[unique(rownames(lm.sum$coefficients)),]
  lm.sum$coefficients <- lm.sum$coefficients[order(rownames(lm.sum$coefficients)),]
  
  # the vector of normalising constants (if applicable)
  if(norm.const == TRUE)
  {
    norm.const.values <- c(rep(sqrt(I/2), sum(grepl("cbpb", rownames(lm.sum$coefficients)))),
                           rep(sqrt(J/2), sum(grepl("rbpb", rownames(lm.sum$coefficients)))),  
                           rep(1/2, sum(grepl("tbpc", rownames(lm.sum$coefficients)))))
    names(norm.const.values) <- rownames(lm.sum$coefficients)[-1]
  } else 
    norm.const.values <- norm.const

  if(!is.null(external))
  {
    lm.sum$coefficients <- rbind(lm.sum$coefficients, lm.init.external)
  }
  
  return(list(Summary = lm.sum, Base = base, Norm.const = norm.const.values, Robust = robust,
              lm = lm.init, Row.levels = all.combinations[[1]][1, ], Col.levels = all.combinations[[2]][1, ]))
}
