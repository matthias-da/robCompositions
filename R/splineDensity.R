#' Estimate density from histogram
#' @rdname smoothSplines
#' @name smoothSplines
#' @import Rcpp 
#' @param k smoothing splines degree
#' @param l order of derivative in the penalization term
#' @param alpha weight for penalization
#' @param data an object of class "matrix" containing data to be smoothed, row by row
#' @param xcp vector of control points
#' @param knots either vector of knots for the splines or a integer for the number of equispaced knots
#' @param weights matrix of weights. If not given, all data points will be weighted the same.
#' @param num_points number of points of the grid where to evaluate the density estimated
#' @param prior prior used for zero-replacements. This must be one of "perks", "jeffreys", "bayes_laplace", "sq" or "default"
#' @param cores number of cores for parallel execution, if the option was enabled before installing the package
#' @param fast 1 if maximal performance is required (print statements suppressed), 0 otherwise
#' @author Alessia Di Blasi, Federico Pavone, Gianluca Zeni, Matthias Templ
#' @return An object of class \code{smoothSpl}, containing among the other the following variables:
#' \item{\code{bspline}}{each row is the vector of B-spline coefficients}
#' \item{\code{Y}}{the values of the smoothed curve, for the grid given}
#' \item{\code{Y_clr}}{the values of the smoothed curve, in the clr setting, for the grid given}
#'
#' @description Given raw (discretized) distributional observations, \code{smoothSplines} computes the density
#' function that 'best' fits data, in a trade-off between smooth and least squares approximation, using B-spline basis functions.
#' @details The original discretized densities are not directly smoothed, but instead the centred logratio transformation is
#' first applied, to deal with the unit integral constraint related to density functions. \cr
#' Then the constrained variational problem is set. This minimization problem for the optimal
#' density is a compromise between staying close to the given data, at the corresponding \code{xcp},
#' and obtaining a smooth function.
#' The non-smoothness measure takes into account the \code{l}th derivative, while the fidelity term is weigthed by \code{alpha}. \cr
#' The solution is a natural spline. The vector of its coefficients is obtained by the minimum norm solution of a linear system.
#' The resulting splines can be either back-transformed to the original Bayes space of density
#' functions (in order to provide their smoothed counterparts for vizualization and interpretation
#' purposes), or retained for further statistical analysis in the clr space.
#' @references J. Machalova, K. Hron & G.S. Monti (2016):
#' Preprocessing of centred logratio transformed density functions
#' using smoothing splines. Journal of Applied Statistics, 43:8, 1419-1435.
#' @examples
#' SepalLengthCm <- iris$Sepal.Length
#' Species <- iris$Species
#'
#' iris1 <- SepalLengthCm[iris$Species==levels(iris$Species)[1]]
#' h1 <- hist(iris1, nclass = 12, plot = FALSE)
#'
#' midx1 <- h1$mids
#' midy1 <- matrix(h1$density, nrow=1, ncol = length(h1$density), byrow=TRUE)
#' knots <- 7
#' \dontrun{
#' sol1 <- smoothSplines(k=3,l=2,alpha=1000,midy1,midx1,knots)
#' plot(sol1)
#' 
#' h1 <- hist(iris1, freq = FALSE, nclass = 12, xlab = "Sepal Length     [cm]", main = "Iris setosa")
#' # black line: kernel method; red line: smoothSplines result
#' lines(density(iris1), col = "black", lwd = 1.5)
#' xx1 <- seq(sol1$Xcp[1],tail(sol1$Xcp,n=1),length.out = sol1$NumPoints)
#' lines(xx1,sol1$Y[1,], col = 'red', lwd = 2)
#' }
#' @export
#' @useDynLib robCompositions, .registration = TRUE
#'

smoothSplines <- function(k,l,alpha,data,xcp,knots,weights = matrix(1, dim(data)[1], dim(data)[2]),num_points = 100, prior = "default", cores = 1, fast = 0)
{

  # Checking if data is a matrix
  if ( !is.matrix(data) )
  {
    err <- simpleError("data must be a matrix type.")
    stop(err)
  }

  # Check weights
  if ( !is.matrix(weights) )
  {
    err <- simpleError("weights must be a matrix type.")
    stop(err)
  }

  if(dim(weights)[1] != dim(data)[1] & dim(weights)[2] != dim(data)[2])
  {
    err <- simpleError("weights size must be equal to data size.")
    stop(err)
  }

  # Converting prior to numeric type
  prior_num <- 0
  if ( prior == "perks" ) prior_num <- 1
  else if ( prior == "jeffreys" ) prior_num <- 2
  else if ( prior == "bayes_laplace" ) prior_num <- 3
  else if ( prior == "sq" ) prior_num <- 4


  # Creating equispaced knots if not given
  if( length(knots) == 1) {
    u <- xcp[1]
    v <- utils::tail(xcp,n=1)
    size <- knots
    step <- (v - u)/(size-1)
    knots_ <- seq(u,v, by = step)
  
    obj <- .Call("smoothingSplines_",as.integer(k),as.integer(l),alpha,
                 data,xcp,knots_,weights,as.integer(num_points),as.integer(prior_num), as.integer(cores), as.integer(fast))
  }
  else
    obj <- .Call("smoothingSplines_",as.integer(k),as.integer(l),alpha,
                 data,xcp,knots,weights,as.integer(num_points),as.integer(prior_num), as.integer(cores), as.integer(fast))

  class(obj) <- "smoothSpl"
  return(obj)
}
