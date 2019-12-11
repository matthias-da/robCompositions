#' Interaction array
#' 
#' Estimates the interaction compositional table 
#' with normalization for further analysis according to Egozcue et al. (2015)
#' 
#' @param x an object of class \dQuote{intTab}
#' @details Estimates the interaction table using its ilr coordinates.
#' @author Matthias Templ
#' @seealso \code{\link{intTab}}
#' @return The interaction array
#' @references 
#' Egozcue, J.J., Pawlowsky-Glahn, V., Templ, M., Hron, K. (2015)
#' Independence in contingency tables using simplicial geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, 44 (18), 3978--3996.
#' 
#' @export
#' @examples 
#' data(precipitation) 
#' tab1prob <- prop.table(precipitation)
#' tab1 <- indTab(precipitation)
#' tabINT <- intTab(tab1prob, tab1)
#' intArray(tabINT)
intArray <- function(x){
  clInfo <- class(x)[1]
  stopifnot(clInfo == "intTab")
  
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
