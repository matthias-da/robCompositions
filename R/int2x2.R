#' Interaction 2x2 table
#' 
#' Estimates the interactions from an 2x2 table under the 
#' null hypotheses of independence.
#' 
#' @param x a 2x2 table
#' @param margin if multidimensional table (larger than 2-dimensional), 
#' then the margin determines on which dimension the independene tables should be estimated.
#' @param pTabMethod to estimate the propability table. Default is \sQuote{dirichlet}. Other available methods: 
#' \sQuote{classical} that is function \code{prop.table()} from package base or method \dQuote{half} that add 1/2 to each cell
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
#' int2x2(employment)
int2x2 <- function(x, margin=3, pTabMethod = c("dirichlet", "half", "classical")){
  ## Matthias Templ, TU WIEN, 17.01.2013 based on Code from Kamila Facevicova from 16.1.2013
  int2tab <- function(x){
    if(!all.equal(dim(x), c(2,2))) stop("ind2.table is for 2x2 tables.")
    xint <- x
    xint[1,1] <- (x[1,1]*x[2,2])^(1/2) 	
    xint[2,1] <- (x[2,1]*x[1,2])^(1/2)
    xint[1,2] <- (x[2,1]*x[1,2])^(1/2)  ## Kamila: is this true??
    xint[2,2] <- (x[1,1]*x[2,2])^(1/2)
    pTab(xint, method="classical")
  }
  dn <- dimnames(x)
  if(length(dim(x)) == 2){
    res <- int2tab(x)
  } else {
    res <- apply(x, margin, int2tab)
    res <- array(res, dim=c(2,2,ncol(res)))
  }
  dimnames(res) <- dn
  class(res) <- "int2x2"
  res	
}