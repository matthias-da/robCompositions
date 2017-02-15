#' Simplicial deviance
#' 
#' @param x a propability table
#' @author Matthias Templ
#' @return The simplicial deviance
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
#' SDev(tab1prob)
SDev <- function(x){
  sum( (log(x/gmean_sum(x)))^2 )
}