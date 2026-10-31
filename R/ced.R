#' Compositional error distance
#'
#' Normalised Aitchison distance between an original and an imputed (or
#' otherwise modified) compositional data set, used to evaluate imputation
#' methods.
#'
#' @details The compositional error distance is the sum of the row-wise
#' Aitchison distances between \code{x} and \code{y}, divided by \code{ni},
#' \deqn{\mathrm{ced}(X, Y) = \frac{1}{n_i}\sum_{i=1}^{n} d_a(x_i, y_i),}{ced(X, Y) = 1/ni * sum_i d_a(x_i, y_i),}
#' where \eqn{d_a} is the Aitchison distance (see \code{\link{aDist}}).
#' Rows that are identical in \code{x} and \code{y} contribute zero. If \code{ni}
#' is the number of imputed cells (e.g. \code{sum(is.na(x))} before imputation, or
#' the number of rounded zeros that were replaced), the result is the average
#' error per imputed value, which makes results comparable across data sets with
#' different numbers of missing values.
#'
#' Columns are matched by position, not by name.
#'
#' @param x matrix or data frame with the original compositions: strictly positive
#' values, no missing values.
#' @param y matrix or data frame of the same dimensions as \code{x}, e.g. the
#' imputed data set; an error is raised if the dimensions differ.
#' @param ni positive number used for normalisation, typically the number of
#' imputed or replaced values.
#' @return The compositional error distance, a single non-negative number. If
#' \code{x} and \code{y} are single-row data frames it is returned as a 1 x 1
#' matrix with the row names as dimnames, as in \code{\link{aDist}}.
#' @author Matthias Templ
#' @references Hron, K., Templ, M., Filzmoser, P. (2010). Imputation of
#' missing values for compositional data using classical and robust methods.
#' \emph{Computational Statistics and Data Analysis}, 54 (12), 3095-3107.
#' \doi{10.1016/j.csda.2009.11.023}
#'
#' Templ, M., Hron, K., Filzmoser, P., Gardlo, A. (2016).
#' Imputation of rounded zeros for high-dimensional compositional data.
#' \emph{Chemometrics and Intelligent Laboratory Systems}, 155, 183-190.
#' \doi{10.1016/j.chemolab.2016.04.011}
#'
#' @seealso \code{\link{aDist}}, \code{\link{rdcm}}, \code{\link{impKNNa}},
#' \code{\link{impCoda}}, \code{\link{imputeBDL}}
#' @export
#' @examples
#' data(expenditures)
#' x <- expenditures
#' x[1, 3] <- NA
#' xi <- impKNNa(x)$xImp
#'
#' # average Aitchison error per imputed value
#' ced(expenditures, xi, ni = sum(is.na(x)))
#'
#' # without imputation error the distance is zero
#' ced(expenditures, expenditures, ni = 1)
ced <- function(x, y, ni){
  return(aDist(x, y)/ni)
}
