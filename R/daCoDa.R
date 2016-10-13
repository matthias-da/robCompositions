#' Linear and quadratic discriminant analysis for compositional data.
#' 
#' Linear and quadratic discriminant analysis for compositional data using either robust or 
#' classical estimation.
#' 
#' An ilr-transformation is applied to compositional data (if \code{coda==TRUE}). For linear 
#' discriminant analysis the functions LdaClassic (classical) and Linda (robust) from the 
#' package rrcov are used. Similarly, quadratic discriminant analysis 
#' uses the functions QdaClassic and QdaCov (robust) from the same package.
#' 
#' The classical linear and quadratic discriminant rules are invariant to ilr and clr
#' transformations. The robust rules are invariant to ilr transformations if
#' affine equivariant robust estimators of location and covariance are taken.
#' 
#' @param x a matrix or data frame containing the explanatory variables
#' @param grp grouping variable: a factor specifying the class for each
#' observation.
#' @param coda TRUE, when the underlying data are compositions.
#' @param method \dQuote{classical} or \dQuote{robust}
#' @param rule a character, either \dQuote{linear} (the default) or \dQuote{quadratic}.
#' @param ... additional arguments for the functions passed through
#' @return An S4 object of class LdaClassic, Linda, QdaClassic or QdaCov. See package 
#' rrcov for details.
#' @author Jutta Gamper
#' @seealso \code{\link[rrcov]{LdaClassic}}, \code{\link[rrcov]{Linda}}, 
#' \code{\link[rrcov]{QdaClassic}}, \code{\link[rrcov]{QdaCov}}
#' @references Filzmoser, P. and Hron, K. and Templ, M. (2012) 
#' Discriminant analysis for compositional data and robust parameter estimation. 
#' \emph{Computational Statistics}, Vol. 27(4), pp. 585-604, 2012.
#' @keywords multivariate
#' @export
#' @importFrom rrcov Linda LdaClassic QdaClassic QdaCov
#' @examples
#' ## toy data (non-compositional)
#' require(MASS)
#' x1 <- mvrnorm(20,c(0,0,0),diag(3))
#' x2 <- mvrnorm(30,c(3,0,0),diag(3))
#' x3 <- mvrnorm(40,c(0,3,0),diag(3))
#' X <- rbind(x1,x2,x3)
#' grp=c(rep(1,20),rep(2,30),rep(3,40))
#' 
#' clas1 <- daCoDa(X, grp, coda=FALSE, method = "classical", rule="linear")
#' summary(clas1)
#' ## predict runs only with newest verison of rrcov
#' \dontrun{
#' predict(clas1)
#' }
#' # specify different prior probabilities
#' clas2 <- daCoDa(X, grp, coda=FALSE, prior=c(1/3, 1/3, 1/3))
#' summary(clas2)
#' 
#' ## compositional data
#' data(coffee)
#' x <- coffee[coffee$sort!="robusta",2:7]
#' group <- droplevels(coffee$sort[coffee$sort!="robusta"])
#' cof.cla <- daCoDa(x, group, method="classical", rule="quadratic")
#' cof.rob <- daCoDa(x, group, method="robust", rule="quadratic")
#' ## predict runs only with newest verison of rrcov
#' \dontrun{
#' predict(cof.cla)@ct
#' predict(cof.rob)@ct
#' }
daCoDa <- function(x, grp, coda=TRUE, method = "classical", rule="linear", ...){
  ## some checks
  if(!class(x) %in% c("matrix", "data.frame")) stop("x must be a matrix or data.frame")
  if(class(x) == "data.frame") x <- as.matrix(x)
  if(length(grp) != dim(x)[1]){
    stop(paste("grp must be of length", dim(x)[1]))
  }
  if(dim(x)[2] < 1){
    stop("matrix or data.frame expected.")
  }
  if(coda){
    x <- pivotCoord(x)
  }
  
  if(rule=="linear" & method=="classical"){
    res <- LdaClassic(x, grp, ...)
  }
  else if(rule=="linear" & method=="robust"){
    res <- Linda(x, grp, method="mcd")
  }
  else if(rule=="quadratic" & method=="classical"){
    res <- QdaClassic(x, grp, ...)
  }
  else if(rule=="quadratic" & method=="robust"){
    res <- QdaCov(x, grp, ...)
  }
  return(res)
}
