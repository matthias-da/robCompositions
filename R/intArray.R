#' Interaction array
#' 
#' Estimates the interaction array
#' 
#' @param x an object of class \dQuote{intTab}
#' @details Estimates the interaction array using an ilr transformation of the interaction table.
#' @author Matthias Templ
#' @seealso \code{\link{intTab}}
#' @return The interaction array
#' @references 
#' Juan Jose Egozcuea, Vera Pawlowsky-Glahn, Matthias Templ, Karel Hron (2015)
#' Independence in Contingency Tables Using Simplicial Geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, Vol. 44 (18), 3978--3996.
#' DOI:10.1080/03610926.2013.824980
#' 
#' @export
#' @examples 
#' data(precipitation) 
#' tab1prob <- prop.table(precipitation)
#' tab1 <- indTab(precipitation)
#' tabINT <- intTab(tab1prob, tab1)
#' intArray(tabINT)
intArray <- function(x){
  stopifnot(class(x) == "intTab")
  
  clr2 <- function(x){
    d <- dim(x)
    x <- as.vector(x)
    D <- length(x)
    res <- numeric(length(D))
    for( i in 1:D ){
      res[i] <- log(x[i]/prod(x[-i])^(1/(D-1))) * (sqrt((D-1)/(D)))^2
    }
    res <- matrix(res, ncol=d[2])
    res/sum(res)
    return(res)
  }
  
  signs <- x$signs
  x <- clr2(x$intTab)
  res <- sum(x^2)
  x^2/res * signs
}
