#' Relative simplicial deviance
#' 
#' @param x a propability table
#' @param y an interaction table
#' @author Matthias Templ
#' @return The relative simplicial deviance
#' @references 
#' Egozcue, J.J., Pawlowsky-Glahn, V., Templ, M., Hron, K. (2015)
#' Independence in contingency tables using simplicial geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, 44 (18), 3978--3996.
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
