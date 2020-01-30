#' Estimate density from histogram - for different \code{alpha}
#'
#' @param k smoothing splines degree
#' @param l order of derivative in the penalization term
#' @param alpha vector of weights for penalization
#' @param data an object of class "matrix" containing data to be smoothed, row by row
#' @param xcp vector of control points
#' @param knots either vector of knots for the splines or a integer for the number of equispaced knots
#' @param weights matrix of weights. If not gives, all data points will be weighted the same.
#' @param prior prior used for zero-replacements. This must be one of "perks", "jeffreys", "bayes_laplace", "sq" or "default"
#' @param cores number of cores for parallel execution
#' @author Alessia Di Blasi, Federico Pavone, Gianluca Zeni, Matthias Templ
#' @return A list of three objects:
#' \item{\code{alpha}}{the values of \code{alpha}}
#' \item{\code{J}}{the values of the functional evaluated in the minimizing}
#' \item{\code{CV-error}}{the values of the leave-one-out CV-error}
#' @description As \code{\link{smoothSplines}}, \code{smoothSplinesVal} computes the density function that 'best' fits
#' discretized distributional data, using B-spline basis functions, for different \code{alpha}. \cr
#' Comparing and choosing an appropriate \code{alpha} is the ultimate goal.
#' @details See \code{\link{smoothSplines}} for the description of the algorithm.
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
#' \dontrun{
#' midx1 <- h1$mids
#' midy1 <- matrix(h1$density, nrow=1, ncol = length(h1$density), byrow=TRUE)
#' knots <- 7
#' sol1 <- smoothSplinesVal(k=3,l=2,alpha=10^seq(-4,4,by=1),midy1,midx1,knots,cores=1)
#' }
## @useDynLib splineDensity
#' @export
smoothSplinesVal <- function(k,l,alpha,data,xcp,knots, weights = matrix(1, dim(data)[1], dim(data)[2]), prior = "default",cores = 1)
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
  if( length(knots) == 1 )
  {
    u <- xcp[1]
    v <- utils::tail(xcp,n=1)
    size <- knots
    step <- (v - u)/(size-1)
    knots_ <- seq(u,v, by = step)
    obj <- .Call("smoothingSplinesValidation_",as.integer(k),as.integer(l),alpha,
                 data,xcp,knots_,weights,as.integer(prior_num), as.integer(cores))
  }
  else
    obj <- .Call("smoothingSplinesValidation_",as.integer(k),as.integer(l),alpha,
                 data,xcp,knots,weights,as.integer(prior_num), as.integer(cores))

  class(obj) <- "smoothSplVal"
  return(obj)
}
