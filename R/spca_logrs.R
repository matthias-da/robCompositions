
#' Function making a matrix of D(D-1) logratios and calculating sparse PCA.
#'
#' @param X a matrix of raw compositional data with "n" rows and "D" columns/components
#' @param alpha a sparsity parameter; the higher its value, the sparser the results; default is 0.01
#' @param beta a tuning parameter resulting in shrinkage of the parameters towards zero; beta = 0 leads to lasso penalty; default is 1e-04
#' @param k number of principal components (PCs) to be calculated; default is (D-1)
#' @param draw a logical parameter stating whether a biplot should be drawn (TRUE) or not (FALSE); default is T
#'
#' @importFrom sparsepca spca
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{X.pairwise}}{A matrix of (D-1) pairwise logratios.}
#'   \item{\code{model}}{A sparse PCA model (using sparsepca::spca) where X.pairwise is the input.}
#'   \item{\code{loadings}}{A matrix of loadings.}
#'   \item{\code{model summary}}{A short summary of the model returning the explained variance by PCs.}
#'   \item{\code{expl.var}}{A proportion of variance of each PC.}
#'   \item{\code{number of zero logratios}}{States how many zero logratios (having zeros in all PCs) are in the model.}
#'   \item{\code{table of all zero}}{Returns the table of all zero logratios.}
#' }
#'
#' @details
#' The function creates a sparse PCA model where a matrix of pairwise logratios is taken as an input. The function spca from the library sparsepca is used for modelling,
#' Erichson et al. (2020) for more details.
#' For detailed information regarding sparse PCA with pairwise logratios, see Nesrstová et al. (2024).
#'
#' @author Viktorie Nesrstová
#'
#' @references
#' Erichson, N.B., Zheng, P. Manohar, K., Brunton, S.L., Kuntz, J.N., Aravkin, Y. (2020). Sparse principal component analysis via variable projection. SIAM J Appl Math. Available at:
#' \url{https://epubs.siam.org/doi/10.1137/18M1211350}
#' DOI: \doi{10.1137/18M1211350}
#'
#' Nesrstová, V., Wilms, I., Hron, K., Filzmoser, P. (2024). Identifying Important Pairwise Logratios in Compositional Data with Sparse Principal Component Analysis. Mathematical Geosciences.  Available at:
#' \url{https://link.springer.com/article/10.1007/s11004-024-10159-0}
#' DOI: \doi{10.1007/s11004-024-10159-0}
#'
#' @examples
#' \dontrun{
#'   if (requireNamespace("MASS", quietly = TRUE)) {
#'
#' # 1. Generate sample data
#' n <- 100              # observations
#' D <- 10               # parts/variables
#' Sig <- diag(D-1)      # positive-definite symmetric matrix -> covariance matrix
#' mu <- c(rep(0, D-1))  # means of variables
#'
#' set.seed(1234)
#' # ilr coordinates
#' Z <- MASS::mvrnorm(n,mu,Sigma = Sig)
#'
#' # Z -> CoDa X
#' V <- compositions::ilrBase(D = D)
#' X <- as.matrix(as.data.frame(acomp(exp(Z%*%t(V)))))
#'
#' # 2. Apply sPCA to pairwise logratios
#' alpha_max <- 1 # specify max value of tuning parameter
#' alpha_nbr <- 50 # specify number of tuning parameters
#' alpha_ratio <- 1000 # specify ratio of largest to smallest tuning parameter
# 'alpha_grid <- c(exp(seq(from = log(alpha_max), to = log(alpha_max/alpha_ratio), length = alpha_nbr)), 0)
#'a <- sort(alpha_grid,decreasing=F)        # zero included
#'
#' # Models for different values of alpha parameters, calculating PC1 and PC2
#' models <- list()
#' for(i in 1:length(a)){
#'  models[[i]] <- spca_logrs(X=X, alpha = a[i], k = 2, draw = F)
#' }
#'
#'   }
#' }
#'
#' @export
spca_logrs <- function(X,alpha = 0.01, beta = 1e-04, k = (D-1), draw = T){

  D <- ncol(X)
  n <- nrow(X)

  X.pairs <- matrix(NA,nrow = n, ncol = D*(D-1))  # matrix of pairwise logratios
  col.names <- c()  # a vector to store colnames of a matrix X.pairs
  ind <- 1
  for(i in 1:D){
    for(j in 1:D){
      if(i!=j){
        logr <- log(X[,i]/X[,j])
        name.logr <- c(paste0("ln(",colnames(X)[i],"/",colnames(X)[j],")"))
        X.pairs[,ind] <- logr
        col.names[ind] <- name.logr
        ind <- ind+1
      } else {
        next
      }
    }
  }
  colnames(X.pairs) <- col.names

  model <- sparsepca::spca(X.pairs, alpha=alpha, beta = beta, k = k, verbose = F)

  # Model loadings:
  P <- model$loadings
  rownames(P) <- colnames(X.pairs)

  # explained variance:
  summary.model <- summary(model)
  expl.var <- summary(model)[3,1:k]  # 3rd line = proportion of variance, cols 1:k

  # How many logratios have zero loadings ("full zero" loadings; all loadings are 0 for a certain logratio)
  isZero <- sum(rowSums(P)==0)
  zeros_sub <- apply(P, 1, function(row) all(row == 0))
  allZero <- P[zeros_sub,]

  # Which variables form non-zero logratios
  row_sub <- apply(P, 1, function(row) any(row !=0 ))  # "all" instead of "any" chooses only those rows without zero (any)
  nonZero <- P[row_sub,]

  # Final output: store a matrix of pairwise logratios and a spca model
  result <- list("X.pairwise" = X.pairs, "model" = model, "loadings" = P, "model summary" = summary.model, "expl.var" = expl.var,
                 "number of zero logratios" = isZero, "table of all zero" = allZero)


}
