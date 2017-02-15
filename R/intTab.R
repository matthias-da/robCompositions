#' Interaction table
#' 
#' Estimates the interaction table based on clr and inverse clr transformations. 
#' 
#' @param x an object of class table
#' @param y the corresponding independence table which is of class \dQuote{intTab}.
#' @param frequencies indicates whether absolute or relative frequencies should be computed.
#' @details Because of the compositional nature of probability tables, the independence tables should 
#' be estimated using geometric margins.
#' @author Matthias Templ
#' @return 
#' \itemize{
#'   \item{intTab}{The interaction table(s) with either relative or absolute frequencies.}
#'   \item{signs}{The sign illustrates if there is an excess of probability (plus), or a 
#' deficit (minus) regarding to the estimated probability table and the independece table in the clr space.}
#' }
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
#' intTab(tab1prob, tab1)
intTab <-
  function(x, y, frequencies = c("relative", "absolute")){
    ## x ... probability table
    ## y ... independence table
    frequency <- match.arg(frequencies)
    if(isTRUE(dim(x) != dim(y))) stop("tables do not have the same dimension")
    if(( round(sum(x, na.rm=TRUE),10) == 1 & round(sum(y, na.rm=TRUE),10) != 1)) 
      stop("x consists of probabilities, y not.")
    if(( round(sum(y, na.rm=TRUE),10) == 1 & round(sum(x, na.rm=TRUE),10) != 1)) 
      stop("y consists of probabilities, x not.")
    n <- sum(x)
    xn <- log(x/gmean_sum(x))
    yn <- log(y/gmean_sum(y))
    res <- xn - yn
    signs <- sign(res)
    res <- exp(res)
    interactionTable <- res / sum(res)
    if(frequency == "absolute") interactionTable <- interactionTable * n
    #intArray <- sgn * interactionTable^2/SDev(interactionTable)
    #list(interactionTable=interactionTable,
    #	 interactionArray=intArray)
    #signs <- sign(x-y)
    res <- list(intTab=interactionTable, signs=signs)
    class(res) <- "intTab"
    return(res)
  }