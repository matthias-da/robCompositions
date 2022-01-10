### Weighted pivot coordinates
####################################################################################################################################################
#' @name weightedPivotCoord
#' @rdname weightedPivotCoord
#' @title Weighted pivot coordinates
#' @author Nikola Stefelova
#' @references Hron K, Filzmoser P, de Caritat P, Fiserova E, Gardlo A (2017) 
#' Weighted 'pivot coordinates for compositional data and their application to geochemical mapping.
#' Mathematical Geosciences 49(6):797-814.
#'  
#' Stefelova N, Palarea-Albaladejo J, and Hron K (2021)
#' Weighted pivot coordinates for PLS-based marker discovery in high-throughput compositional data.
#' Statistical Analysis and Data Mining: The ASA Data Science Journal 14(4):315-330.
#'  
#' @description Weighted pivot coordinates as a special case of isometric logratio coordinates. 
#' 
#' @param x object of class `data.frame` or `matrix`; positive values only
#' @param  pivotvar pivotal variable; if any other number than 1, the data are resorted in that sense that 
#' pivotvar is shifted to the first part
#' @param option option for the choice of weights. If `option = "var"` (default), weights are based on variation matrix elements: 
#' `(1/t_{1j})^pow`, if `option = "cor"`, weights are based on correlations between variable specified in yvar and logratios and its distribution:
#' `|integral_{0}^{r_{j}} f(x) dx|`, `f(x)...` Kernel density estimator for `s_{j}; s_{j}=0 if |r_{j}|<cut` otherwise `s_{j}=r_{j}`,
#' `cut = min(#r_{j}=>0/#r_{j}, #r_{j}<0/#r_{j}`, with Gaussian Kernel function and bandwidth `h=0.05`.
#' @param method method for estimation of variation/correlation,
#' if `option = "classical"` (default), classical estimation is applied,
#' if `option = "robust"`, robust estimation is applied;
#' @param pow if `option = "var"`, power `pow` is applied on unnormalized weights; default is 1;
#' @param yvar if `option = "cor"`, weights are based on correlation between logratios and variable specified in `yvar`;
#' @details Weighted pivot coordinates map D-part compositional data from the simplex into a (D-1)-dimensional real space isometrically.
#'  The relevant relative information about one of parts is contained in the first coordinate.
#'  Unlike in the (ordinary) pivot coordinates, the pairwise logratios aggregated into the first coordinate are weighted according to their relevance for the purpose of the analysis.
#' @keywords multivariate
#' @export
#' @seealso 
#' \code{\link{pivotCoord}} 
#' @return 
#' \item{WPC}{weighted pivot coordinates (matrix with n rows and (D-1) columns)}
#' \item{w}{logcontrasts (matrix with D rows and (D-1) columns)}
#' @importFrom robustHD corHuber
#' @importFrom utils combn
#' @importFrom stats integrate
#' @examples 
#' ###################
#' data(phd)
#' x <- phd[, 7:ncol(phd)]
#' x[x == 0] <- 0.1 # better: impute with one 
#'                  # of the zero imputation methods 
#'                  # from robCompositions
#' 
#' # first variable as pivotal, weights based on variation matrix
#' wpc_var <- weightedPivotCoord(x)
#' coordinates <- wpc_var$WPC
#' logcontrasts <- wpc_var$w
#'
#' # third variable as pivotal, weights based on variation matrix, 
#' # robust estimation of variance, effect of weighting enhanced
#' wpc_var <- weightedPivotCoord(x, pivotvar = 3, method = "robust", pow = 2)
#' coordinates = wpc_var$WPC
#' logcontrasts = wpc_var$w
#'
#' # first variable as pivotal, weights based on correlation between pairwise logratios and y
#' wpc_cor <- weightedPivotCoord(x, option = "cor", yvar = phd$female)
#' coordinates <- wpc_cor$WPC
#' logcontrasts <- wpc_cor$w
#'
#'# fifth variable as pivotal, weights based on correlation between pairwise logratios 
#'# and y, robust estimation of correlation
#' wpc_cor <- weightedPivotCoord(x, pivotvar = 5, option = "cor", method = "robust", yvar = phd$female)
#' coordinates <- wpc_cor$WPC
#' logcontrasts <- wpc_cor$w
#' 
weightedPivotCoord <- function(x, pivotvar = 1, option = "var", method = "classical", 
                            pow = 1, yvar = NULL){
  
  ############################################################################################################
  # the norm of a vector
  norm_vec = function(x) sqrt(sum(x^2))
  
  # functions to compute coefficients for construction of weighted coordinates
  
  # w_{1}
  vecw1 = function(weights){
    D = length(weights)
    vec = c(1, -weights[-1])
    norm = norm_vec(vec)
    ww1 = vec/norm
    return(ww1)
  }
  
  # w_{2}
  vecw2 = function(weights){
    D = length(weights)
    vec = c(rep(0, D-3), weights[D-1]-weights[D], weights[D]-weights[D-2], weights[D-2]-weights[D-1])
    norm = norm_vec(vec)
    ww2 = vec/norm
    return(ww2)
  }
  
  # w_{p}, p = 3,...,D-2
  vecwp = function(weights, p){
    D = length(weights)
    s1 = 0
    for(i in (D-p+1):(D-1)){
      for(j in (i+1):D){
        s1 = s1+weights[i]*weights[j]
      }
    }
    s2 = sum(weights[(D-p+1):D]^2)
    s3 = sum(weights[(D-p+1):D])
    vec = rep(0, D)
    vec[D-p] = 2*s1-(p-1)*s2
    for (k in (D-p+1):D){
      vec[k] = s2-weights[k]^2-(weights[D-p]+weights[k])*(s3-weights[k])+(p-1)*weights[k]*weights[D-p]
    }
    norm = norm_vec(vec)
    wwp = vec/norm
    return(wwp)
  }
  
  # w_{D-1}
  vecwDm1 = function(weights){
    D = length(weights)
    s1 = 0
    for(i in 2:(D-1)){
      for(j in (i+1):D){
        s1 = s1+weights[i]*weights[j]
      }
    }
    s2 = sum(weights[-1]^2)
    s3 = sum(weights[-1])
    vec = rep(0, D)
    vec[1] = 2*s1-(D-2)*s2
    for (k in 2:D){
      vec[k] = (s2-weights[k]^2)+(1-weights[k])*(s3-weights[k])-(D-2)*weights[k]
    }
    norm = norm_vec(vec)
    wwDm1 = vec/norm
    return(wwDm1)
  }
  
  WCoef = function(weights){
    D = length(weights)
    WW = matrix(0, D, D-1)
    WW[, 1] = vecw1(weights = weights)
    WW[, 2] = vecw2(weights = weights)
    for(k in 3:(D-2)){
      WW[, k] = vecwp(weights = weights, p = k)
    }
    WW[, D-1] = vecwDm1(weights = weights)
    return(WW)
  }
  
  # Kernel function
  gauss = function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
  
  
  if (option == "var") {
    x1 = cbind(x[,pivotvar], x[,-pivotvar]) 
    if (method == "classical") {
      #tj1 = compositions::variation(acomp(x1))[,1]
      tj1 = robCompositions::variation(x1, method = "Pivot")[,1]
    } else if (method == "robust") {
      #tj1 = robCompositions::variation(acomp(x1))[,1]
      tj1 = robCompositions::variation(x1)[,1]
    }
    weights2w = 1/tj1[-1]^pow
  } else if (option == "cor") {
    x1 = cbind(x[,pivotvar], x[,-pivotvar]) 
    colnames(x1)[1] = colnames(x)[pivotvar]
    cn = colnames(x1)
    combinations = combn(cn, 2, simplify = FALSE)[1:((ncol(x))-1)]
    codalr = as.data.frame(lapply(combinations, function(j, a) log(a[, j[1]]/a[, j[2]]), a = x1))
    rrj = NULL
    for(j in 1:ncol(codalr)){
      if (method == "classical") {
        rj = cor(yvar, codalr[,j])
      } else if (method == "robust") {
        rj = corHuber(yvar, codalr[,j])
      }
      rrj = c(rrj, rj)
    }
    ssj = rrj
    cut = round(2*min(mean(rrj>=0), mean(rrj<0)), 1)
    ssj[abs(rrj)<cut] = 0
    KernelEstC = function(x){
      x_i = ssj
      h = 0.05
      n = length(x_i)
      return(sum(gauss((x-x_i)/h))/(h*n))
    }
    weights2w = NULL
    D = ncol(x1)
    for(k in 1:(D-1)){
      weights2w = c(weights2w, abs(integrate(f=Vectorize(KernelEstC), lower=0 , upper=rrj[k])$value))
    }
    weights2ww = weights2w  
    for(k in 1:(D-1)){
      vvk = round(weights2w[k], 4)
      vv = round(weights2w[-k], 4)
      if(sum(vv==vvk)>0) weights2ww[k] = weights2ww[k]+abs(rrj[k])/1000
    }
    weights2w = weights2ww
  }
  weights2 = weights2w/sum(weights2w)
  weights1 = c(1, weights2)
  WW = WCoef(weights = weights1)
  WPC = log(as.matrix(x1))%*%WW
  return(list("WPC" = WPC, "w" = WW))
}



