#' compositional error deviation
#' 
#' Normalized Aitchison distance between two data sets
#'  
#' @param x matrix or data frame
#' @param y matrix or data frame of the same size as x 
#' @param ni normalization parameter. See details below.
#' @return the compositinal error distance
#' @author Matthias Templ
#' @references Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of
#' missing values for compositional data using classical and robust methods
#' \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages
#' 3095-3107.
#' 
#' Templ, M. and Hron, K. and Filzmoser and Gardlo, A. (2016). 
#' Imputation of rounded zeros for high-dimensional compositional data. 
#' \emph{Chemometrics and Intelligent Laboratory Systems}, 54 (12) 3095-3107.
#' 
#' @seealso \code{\link{rdcm}}
#' @details This function has been mainly written for procudures 
#' that evaluate imputation or replacement of rounded zeros. The ni parameter can thus, e.g. be
#' used for expressing the number of rounded zeros.
#' @keywords manip
#' @export
#' @examples
#' data(expenditures)
#' x <- expenditures
#' x[1,3] <- NA
#' xi <- impKNNa(x)$xImp
#' ced(expenditures, xi, ni = sum(is.na(x)))
ced <- function(x, y, ni){
  return(aDist(x, y)/ni)
}