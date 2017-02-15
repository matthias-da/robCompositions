#' Relative simplicial deviance tests
#' 
#' Monte Carlo based contingency table tests considering the compositional approach to contingency tables.
#' 
#' @param x matrix, data.frame or table
#' @param R an integer specifying the number of replicates used in the Monte Carlo test.
#' @param method either \dQuote{rmultinom} (default) or \dQuote{permutation}. 
#' @details Method \dQuote{rmultinom} generate multinomially distributed samples  
#' from the independent probability table, which is estimated from \code{x} using geometric mean marginals. 
#' The relative simplicial deviance of the original data are then compared to the generated ones.
#' 
#' Method \dQuote{permutation} permutes the entries of \code{x} and compares the relative simplicial deviance estimated from
#' the original data to the ones of the permuted data (the independence table is unchanged and originates on \code{x}). 
#' 
#' Method \dQuote{rmultinom} should be preferred, while method \dQuote{permutation} can be used for comparisons. 
#' @author Matthias Templ, Karel Hron
#' @return 
#' A list with class  \dQuote{htest} containing the following components:
#' \itemize{
#'   \item{statistic}{the value of the relative simplicial deviance (test statistic).}
#'   \item{method}{a character string indicating what type of rSDev.test was performed.}
#'   \item{p.value}{the p-value for the test.}
#' }
#' @seealso \code{\link{rSDev}}
#' @references 
#' Juan Jose Egozcuea, Vera Pawlowsky-Glahn, Matthias Templ, Karel Hron (2015)
#' Independence in Contingency Tables Using Simplicial Geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, Vol. 44 (18), 3978--3996.
#' DOI:10.1080/03610926.2013.824980
#' 
#' @export
#' @keywords htest
#' @examples 
#' data(precipitation)
#' rSDev.test(precipitation)
rSDev.test <- function(x, R=999, method="multinom") {   
  DNAME <- deparse(substitute(x))	
  if( R < 1 ) stop("choose a higher value for R")
  if( R < 50 ) warnings("maybe, the estimation of the p-value(s) is not accurate; choose a higher value for R")
  id <- indTab(x) ## internally pTab with dirichlet.
  
  if(method == "multinom"){
    n=sum(x)
    orig <- rSDev(pTab(x), id) #idx
    gen <- replicate(R, rSDev(pTab(matrix(rmultinom(1,n,id), ncol=ncol(x))), id) )
    pvalue <- mean(gen > orig, na.rm=TRUE)
    tname <- "Multinomial relative simplicial deviance Monte Carlo test"
  } 
  if(method == "permutation"){
    xi <- matrix(sample(x), ncol=ncol(x))
    orig <- rSDev(x, id)
    gen <- replicate(R, rSDev(         xi, id))	
    pvalue <- mean(gen > orig)
    tname <- "Permutation relative simplicial deviance Monte Carlo test"
  }
  if(method == "MC"){
    orig <- rSDev(x,id)
    z <- NULL
    ## under the null of normal distributionsthe
    ## statistics have a chisq-distribution:
    z <- rchisq(n=R, df=(nrow(x)-1)*(ncol(x)-1))
    pvalue <- mean(z > orig)
    tname <- "Parametric relative simplicial deviance Monte Carlo test"
    
  }
  #	p <- sapply(X=1:R, FUN=function(X,...){ stat(rnorm(n, mv, varmat), location=location) })
  
  ## confidence interval for p-value:
  lci <- qnorm(0.975) * sqrt(( pvalue*(1-pvalue)/R))
  cint <- c(pvalue-lci, pvalue+lci)
  RVAL <- list(statistic = c(stat = orig), 
               method = tname, 
               p.value=pvalue, conf.int=cint,  data.name = DNAME)	
  class(RVAL) <- "htest"
  return(RVAL)	
}