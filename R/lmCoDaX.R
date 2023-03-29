#' @importFrom stats lm
#' @importFrom robustbase lmrob.control
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#' @importFrom dplyr all_of
#' @importFrom dplyr relocate
#' @importFrom dplyr rename_with
#' @importFrom dplyr mutate_if
NULL

#' Classical and robust regression of non-compositional (real) response on
#' compositional and non-compositional predictors
#' 
#' Delivers appropriate inference for regression of y on a compositional matrix
#' X or and compositional and non-compositional combined predictors.
#' 
#' Compositional explanatory variables should not be directly used in a linear
#' regression model because any inference statistic can become misleading.
#' While various approaches for this problem were proposed, here an approach
#' based on the pivot coordinates is used. Further these compositional explanatory 
#' variables can be supplemented with external non-compositional data
#' and factor variables.
#' 
#' @aliases lmCoDaX ilrregression robilrregression
#' @param y The response which should be non-compositional
#' @param X The compositional and/or non-compositional predictors as a matrix, data.frame or numeric
#' vector
#' @param external Specify the columns name of the external variables. The name has to be introduced as follows:
#' external = c("variable_name"). Multiple selection is supported for the external variable. Factor variables are
#' automatically detected.
#' @param method If robust, LTS-regression is applied, while with method equals
#' \dQuote{classical}, the conventional least squares regression is applied.
#' @param pivot_norm if FALSE then the normalizing constant is not used, if TRUE sqrt((D-i)/(D-i+1))
#' is used (default). The user can also specify a self-defined constant.
#' @param max_refinement_steps (for the fast-S algorithm): maximal number of refinement
#' steps for the fully iterated best candidates.
#' @return An object of class \sQuote{lts} or \sQuote{lm} and two summary
#' objects.
#' @author Peter Filzmoser, Roman Wiedemeier, Matthias Templ
#' @seealso \code{\link{lm}}
#' @references Filzmoser, P., Hron, K., Thompsonc, K. (2012) Linear regression
#' with compositional explanatory variables. \emph{Journal of Applied
#' Statistics}, 39, 1115-1128.
#' @keywords models
#' @export
#' @examples
#' 
#' ## How the total household expenditures in EU Member
#' ## States depend on relative contributions of 
#' ## single household expenditures:
#' data(expendituresEU)
#' y <- as.numeric(apply(expendituresEU,1,sum))
#' lmCoDaX(y, expendituresEU, method="classical")
#' 
#' ## How the relative content of sand of the agricultural
#' ## and grazing land soils in Germany depend on
#' ## relative contributions of the main chemical trace elements,
#' ## their different soil types and the Annual mean temperature:
#' data("gemas")
#' gemas$COUNTRY <- as.factor(gemas$COUNTRY)
#' gemas_GER <- dplyr::filter(gemas, gemas$COUNTRY == 'POL')
#' ssc <- cenLR(gemas_GER[, c("sand", "silt", "clay")])$x.clr
#' y <- ssc$sand
#' X <- dplyr::select(gemas_GER, c(MeanTemp, soilclass, Al:Zr))
#' X$soilclass <- factor(X$soilclass)
#' lmCoDaX(y, X, external = c('MeanTemp', 'soilclass'),
#' method='classical', pivot_norm = 'orthonormal')
#' lmCoDaX(y, X, external = c('MeanTemp', 'soilclass'),
#' method='robust', pivot_norm = 'orthonormal')
lmCoDaX <- function (y, X, external = NULL, method = "robust", pivot_norm = 'orthonormal', 
                     max_refinement_steps = 200) { # ltsReg mit lmrob ersetzen und dann sollte die Fehlermeldung verschwinden
  
  if (!is.null(external) & (typeof(external) != "character")) {
    stop("Invalid datatype for external")
  }
  if ((any(external == names(X %>% select_if(is.factor)))) == FALSE & (any(sapply(X, function(x) !is.numeric(x))))) {
    stop("Variable with datatype character or factor have to be defined as external")
  }
  if (any(is.na(y))){
    dat <- cbind(y, X)
    dat_missing <- dat %>% dplyr::filter(is.na(y))
    n <- dim(dat_missing)[1]
    dat_new <- dat %>% dplyr::filter(!is.na(y))
    
    X <- dat_new %>% dplyr::select(-c(y))
    y <- dat_new %>% dplyr::select(c(y))
  }
  if (!is.null(external) & (any(sapply(X, function(x) is.numeric(x))))){
    external_col <- X %>% dplyr::select(all_of(external)) %>% select_if(is.numeric)
    if (all(sapply(external_col, function(x) is.numeric(x)))){
      n_externals <- length(external_col)
    } else {
      stop("Datatype of all external non-factor variables have to be numeric")
    }
  } else {
    external_col = NULL
  }
  if (!is.null(external) & (any(sapply(X, function(x) !is.numeric(x))))) {
    X <- X %>% mutate_if(sapply(X, is.character), as.factor)
    factor_var <- X %>% dplyr::select(all_of(external)) %>% select_if(is.factor)
    factor_col <- factor_var[, 1]
    if (is.factor(factor_col) | is.character(factor_col)){
      n_levels <- length(unique(as.character(factor_col)))
    } else {
      stop("Datatype of factor variable has to be factor or character")
    }
    # if (any(is_empty(unique(as.character(factor_col)), first.only = FALSE, all.na.empty = TRUE) == TRUE)){
      empty_levels <- function(x){
        e <- levels(x)[table(x) == 0]
        # Print the empty levels (if any)
        if (length(e) > 0) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    if(any(is.na(levels(factor_col))) | empty_levels(factor_col)){
      stop("The factor variable provided contains levels with \nmissing values or empty levels. Please correct.")
    }
  } else {
    factor_col = NULL
    factor_var <- NULL
  }
  if (!is.null(factor_col) & length(factor_var) > 1) {
    stop("There are more than 1 factor variable defined")
  }
  
  ilrregression <- function(X, y, external, pivot_norm) {
    
    if (!is.null(factor_col) & !is.null(external_col)){
      X_selected <- X %>% dplyr::select(-all_of(c(external)))
      ZV <- data.frame(cbind(y, X %>% relocate(all_of(c(names(factor_var), names(external_col)))) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(all_of(external))), function(x){paste0("External.", x)}) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(-all_of(c(external)))), function(x){paste0("Internal.", x)})))
      lmcla <- lm(y ~ ., data = ZV)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:(ncol(X)-1-n_externals)) {
        Zj <- pivotCoord(cbind(X_selected[, j], X_selected[, -j]), method = pivot_norm)
        ZVj <- data.frame(Factor = factor_col, Externals = external_col, Z = Zj)
        dj <- data.frame(y = y, Z = ZVj)
        res <- lm(y ~ ., data = dj)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:(n_levels+n_externals+1), ] <- res.sum$coefficients[1:(n_levels+n_externals+1), ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
          ilr.sum$fstatistic <- res.sum$fstatistic
        }
        else {
          ilr.sum$coefficients[j + n_levels + n_externals, ] <- res.sum$coefficients[(n_levels+n_externals+1), ]
          
        }
      }
    }
    if (is.null(factor_col) & is.null(external_col)){
      # ZV <- data.frame(cbind(y, X %>% rename_with(.cols = tidyselect::everything(), function(x){paste0("Internal.", x)})))
      ZV <- data.frame(y, X)
      colnames(ZV)[2:ncol(ZV)] <- paste0("Internal.", colnames(ZV)[2:ncol(ZV)])
      lmcla <- lm(y ~ ., data = ZV)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:ncol(X)) {
        Zj <- pivotCoord(cbind(X[, j], X[, -j]), method = pivot_norm)
        dj <- data.frame(y = y, Z = Zj)
        res <- lm(y ~ ., data = dj)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:2, ] <- res.sum$coefficients[1:2, ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
          ilr.sum$fstatistic <- res.sum$fstatistic
        }
        else {
          ilr.sum$coefficients[j + 1, ] <- res.sum$coefficients[2, ]
        }
      }
    }
    if (!is.null(factor_col) & is.null(external_col)){
      X_selected <- X %>% dplyr::select(-all_of(c(external)))
      ZV <- data.frame(cbind(y, X %>% relocate(all_of(c(names(factor_var)))) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(all_of(external))), function(x){paste0("External.", x)}) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(-all_of(c(external)))), function(x){paste0("Internal.", x)})))
      lmcla <- lm(y ~ ., data = ZV)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:(ncol(X)-1)) {
        Zj <- pivotCoord(cbind(X_selected[, j], X_selected[, -j]), method = pivot_norm)
        ZVj <- data.frame(Factor = factor_col, Z = Zj)
        dj <- data.frame(y = y, Z = ZVj)
        res <- lm(y ~ ., data = dj)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:(n_levels+1), ] <- res.sum$coefficients[1:(n_levels+1), ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
          ilr.sum$fstatistic <- res.sum$fstatistic
        }
        else {
          ilr.sum$coefficients[j + n_levels, ] <- res.sum$coefficients[(n_levels+1), ]
        }
      }
    }
    if (is.null(factor_col) & !is.null(external_col)){
      X_selected <- X %>% dplyr::select(-all_of(c(external)))
      ZV <- data.frame(cbind(y, X %>% relocate(all_of(c(names(external_col)))) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(all_of(external))), function(x){paste0("External.", x)}) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(-all_of(c(external)))), function(x){paste0("Internal.", x)})))
      lmcla <- lm(y ~ ., data = ZV)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:(ncol(X)-n_externals)) {
        Zj <- pivotCoord(cbind(X_selected[, j], X_selected[, -j]), method = pivot_norm)
        ZVj <- data.frame(Externals = external_col, Z = Zj)
        dj <- data.frame(y = y, Z = ZVj)
        res <- lm(y ~ ., data = dj)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:(n_externals+2), ] <- res.sum$coefficients[1:(n_externals+2), ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
          ilr.sum$fstatistic <- res.sum$fstatistic
        }
        else {
          ilr.sum$coefficients[j + n_externals + 1, ] <- res.sum$coefficients[(n_externals+2), ]
        }
      }
    }
    list(lm = lmcla, lm = lmcla.sum, ilr = ilr.sum)
  }
  
  robilrregression <- function(X, y, external, pivot_norm) {
    cont_lmrob <- lmrob.control(fast.s.large.n = Inf, k.max = max_refinement_steps)
    
    if (!is.null(factor_col) & !is.null(external_col)){
      X_selected <- X %>% dplyr::select(-all_of(c(external)))
      ZV <- data.frame(cbind(y, X %>% relocate(all_of(c(names(factor_var), names(external_col)))) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(all_of(external))), function(x){paste0("External.", x)}) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(-all_of(c(external)))), function(x){paste0("Internal.", x)})))
      lmcla <- robustbase::lmrob(y ~ ., data = ZV, control = cont_lmrob)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:(ncol(X)-1-n_externals)) {
        Zj <- pivotCoord(cbind(X_selected[, j], X_selected[, -j]), method = pivot_norm)
        ZVj <- data.frame(Factor = factor_col, Externals = external_col, Z = Zj)
        dj <- data.frame(y = y, Z = ZVj)
        res <- robustbase::lmrob(y ~ ., data = dj, control = cont_lmrob)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:(n_levels+n_externals+1), ] <- res.sum$coefficients[1:(n_levels+n_externals+1), ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
        }
        else {
          ilr.sum$coefficients[j + n_levels + n_externals, ] <- res.sum$coefficients[(n_levels+n_externals+1), ]
        }
      }
    }
    if (is.null(factor_col) & is.null(external_col)){
      # ZV <- data.frame(cbind(y, X %>% rename_with(.cols = everything(), function(x){paste0("Internal.", x)})))
      ZV <- data.frame(y, X)
      colnames(ZV)[2:ncol(ZV)] <- paste0("Internal.", colnames(ZV)[2:ncol(ZV)])
      lmcla <- robustbase::lmrob(y ~ ., data = ZV, control = cont_lmrob)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:ncol(X)) {
        Zj <- pivotCoord(cbind(X[, j], X[, -j]), method = pivot_norm)
        dj <- data.frame(y = y, Z = Zj)
        res <- robustbase::lmrob(y ~ ., data = dj, control = cont_lmrob)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:2, ] <- res.sum$coefficients[1:2, ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
        }
        else {
          ilr.sum$coefficients[j + 1, ] <- res.sum$coefficients[2, ]
        }
      }
    }
    if (!is.null(factor_col) & is.null(external_col)){
      X_selected <- X %>% dplyr::select(-all_of(c(external)))
      ZV <- data.frame(cbind(y, X %>% relocate(all_of(c(names(factor_var)))) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(all_of(external))), function(x){paste0("External.", x)}) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(-all_of(c(external)))), function(x){paste0("Internal.", x)})))
      lmcla <- robustbase::lmrob(y ~ ., data = ZV, control = cont_lmrob)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:(ncol(X)-1)) {
        Zj <- pivotCoord(cbind(X_selected[, j], X_selected[, -j]), method = pivot_norm)
        ZVj <- data.frame(Factor = factor_col, Z = Zj)
        dj <- data.frame(y = y, Z = ZVj)
        res <- robustbase::lmrob(y ~ ., data = dj, control = cont_lmrob)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:(n_levels+1), ] <- res.sum$coefficients[1:(n_levels+1), ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
        }
        else {
          ilr.sum$coefficients[j + n_levels, ] <- res.sum$coefficients[n_levels + 1, ]
        }
      }
    }
    if (is.null(factor_col) & !is.null(external_col)){
      X_selected <- X %>% dplyr::select(-all_of(c(external)))
      ZV <- data.frame(cbind(y, X %>% relocate(all_of(c(names(external_col)))) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(all_of(external))), function(x){paste0("External.", x)}) %>%
                               rename_with(.cols = colnames(X %>% dplyr::select(-all_of(c(external)))), function(x){paste0("Internal.", x)})))
      lmcla <- robustbase::lmrob(y ~ ., data = ZV, control = cont_lmrob)
      lmcla.sum <- summary(lmcla)
      ilr.sum <- lmcla.sum
      
      for (j in 1:(ncol(X)-n_externals)) {
        Zj <- pivotCoord(cbind(X_selected[, j], X_selected[, -j]), method = pivot_norm)
        ZVj <- data.frame(Externals = external_col, Z = Zj)
        dj <- data.frame(y = y, Z = ZVj)
        res <- robustbase::lmrob(y ~ ., data = dj, control = cont_lmrob)
        res.sum <- summary(res)
        if (j == 1) {
          ilr.sum$coefficients[1:(n_externals+2), ] <- res.sum$coefficients[1:(n_externals+2), ]
          ilr.sum$residuals <- res.sum$residuals
          ilr.sum$sigma <- res.sum$sigma
          ilr.sum$r.squared <- res.sum$r.squared
          ilr.sum$adj.r.squared <- res.sum$adj.r.squared
        }
        else {
          ilr.sum$coefficients[j + n_externals + 1, ] <- res.sum$coefficients[(n_externals+2), ]
        }
      }
    }
    list(lm = lmcla, lm = lmcla.sum, ilr = ilr.sum)
  }
  
  if (method == "classical") {
    reg <- ilrregression(X, y, external, pivot_norm)
  }
  else if (method == "robust") {
    reg <- robilrregression(X, y, external, pivot_norm)
  }
  if (exists("dat_missing")) {
    message("There are ", n ," observations omitted due to missings in the target variable")
  }
  return(reg)
}