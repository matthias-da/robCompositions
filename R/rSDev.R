#' Relative simplicial deviance
#' 
#' @param x a propability table
#' @param y an interaction table
#' @author Matthias Templ
#' @return The relative simplicial deviance
#' @references 
#' Juan Jose Egozcuea, Vera Pawlowsky-Glahn, Matthias Templ, Karel Hron (2015)
#' Independence in Contingency Tables Using Simplicial Geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, Vol. 44 (18), 3978--3996.
#' DOI:10.1080/03610926.2013.824980
#' 
#' @export
#' @examples 
#' data(precipitation) 
#' tabprob <- prop.table(precipitation)
#' tabind <- indTab(precipitation)
#' tabint <- intTab(tabprob, tabind)
#' rSDev(tabprob, tabint$intTab)
rSDev <- function(x, y){
  ## x ... probability table
  #	## y ... interaction table
  ## y ... independence table
  ### TODO: use the SDev of the interaction divided by the SDev of x
  ### --> to be correct for arithmetic marginals
  (SDev(x) - SDev(y)) / SDev(x)
}