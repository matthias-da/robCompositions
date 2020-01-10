#' Q-mode cluster analysis for compositional parts
#' 
#' Clustering using the variation matrix of compositional parts
#' 
#' @aliases clustCoDa_qmode plot.clustCoDa_qmode 
#' @param x compositional data represented as a data.frame
#' @param method hclust method
#' @return a hclust object
#' @author Matthias Templ (accessing the basic features of hclust that 
#' are all written by other authors)
#' @export
#' @references Filzmoser, P., Hron, K. Templ, M. (2018)
#' \emph{Applied Compositional Data Analysis}, 
#' Springer, Cham.
#' 
#' @keywords multivariate
#' @importFrom stats hclust
#' @examples
#' data(expenditures) 
#' x <- expenditures
#' cl <- clustCoDa_qmode(x)
#' \dontrun{
#' require(reshape2)
#' plot(cl)
#' cl2 <- clustCoDa_qmode(x, method = "single")
#' plot(cl2)
#' }
clustCoDa_qmode <- function(x, method="ward.D2"){
  d <- as.dist(variation(x))
  clust <- hclust(d, method) 
  return(clust)
}
