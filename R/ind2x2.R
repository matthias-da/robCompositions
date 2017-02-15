#' Independence 2x2 table
#' 
#' Estimates the expected frequencies from an 2x2 table under the 
#' null hypotheses of independence.
#' 
#' @param x a 2x2 table
#' @param margin if multidimensional table (larger than 2-dimensional), 
#' then the margin determines on which dimension the independene tables should be estimated.
#' @param pTabMethod \sQuote{classical} that is function \code{prop.table()} from package base or method \dQuote{half} that add 1/2 to each cell
#' to avoid zero problems.
#' @author Kamila Facevicova, Matthias Templ
#' @return The independence table(s) with either relative or absolute frequencies.
#' @references 
#' K. Facevicova, K. Hron, V. Todorov, D. Guo, M. Templ (2014).
#' Logratio approach to statistical analysis of 2x2 compositional tables.
#' \emph{Journal of Applied Statistics}, Volume 41 (5), 944--958.
#' DOI:10.1080/02664763.2013.856871
#' @export
#' @examples 
#' data(employment) 
#' ind2x2(employment)
ind2x2 <- function(x, margin=3, pTabMethod = c("dirichlet", "half", "classical")){
  ## Matthias Templ, TU WIEN, 17.01.2013
  pTabMethod <- match.arg(pTabMethod)
  ind2tab <- function(x){
    ## Matthias Templ, TU WIEN, 17.01.2013
    if(!all.equal(dim(x), c(2,2))) stop("ind2.table is for 2x2 tables.")
    xind <- x
    xind[1,1] <- x[1,1]*(x[2,1]*x[1,2])^(1/2) 	
    xind[2,1] <- x[2,1]*(x[1,1]*x[2,2])^(1/2)
    xind[1,2] <- x[1,2]*(x[1,1]*x[2,2])^(1/2)
    xind[2,2] <- x[2,2]*(x[2,1]*x[1,2])^(1/2)
    pTab(xind, method = pTabMethod)
  }
  dn <- dimnames(x)
  if(length(dim(x)) == 2){
    res <- ind2tab(x)
  } else {
    res <- apply(x, margin, ind2tab)
    res <- array(res, dim=c(2,2,ncol(res)))
  }
  dimnames(res) <- dn
  class(res) <- "ind2x2"
  res	
}