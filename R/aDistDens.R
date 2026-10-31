#' Aitchison distance between densities
#' 
#' Computes the distance between probability densities in the Bayes Hilbert
#' space, i.e. the \eqn{L^2} distance of their functional clr transforms. It is
#' the functional counterpart of the Aitchison distance \code{\link{aDist}} for
#' compositions.
#' 
#' @details For densities \eqn{f} and \eqn{g} with respect to the Lebesgue
#' measure on \eqn{[a, b]}, the distance is
#' \deqn{d(f, g)^2 = \int_a^b \left(\mathrm{fcenLR}(f)(t) - \mathrm{fcenLR}(g)(t)\right)^2 dt,}{%
#'   d(f, g)^2 = integral_a^b (fcenLR(f)(t) - fcenLR(g)(t))^2 dt,}
#' see \code{\link{fcenLR}}. If the density \code{p} of a reference measure
#' \eqn{P} is given, \code{f} and \code{g} are \eqn{P}-densities and the
#' distance of the weighted Bayes space \eqn{B^2(P)} is computed,
#' \deqn{d_P(f, g)^2 = \int \left(\mathrm{fcenLR}_P(f)(t) - \mathrm{fcenLR}_P(g)(t)\right)^2 p(t)\,dt,}{%
#'   d_P(f, g)^2 = integral (fcenLR_P(f)(t) - fcenLR_P(g)(t))^2 p(t) dt,}
#' see \code{\link{fcenLRp}}.
#' 
#' The integrals are approximated by the trapezoidal rule (\code{\link{trapzc}})
#' on the equidistant grid \code{z}. On this grid the distance equals the
#' weighted Aitchison distance \code{\link{aDist}(f, g, weights = c * p)} of the
#' density values, where \eqn{c = h (1/2, 1, \dots, 1, 1/2)} are the trapezoidal
#' weights for spacing \eqn{h}; \code{aDistDens} uses exactly this computation.
#' Like the Aitchison distance, the result does not change if a density is
#' multiplied by a positive constant, so the densities do not need to be
#' normalised.
#' 
#' The distance follows the convention of Egozcue and Pawlowsky-Glahn (2016),
#' without the factor \eqn{1/P(\Omega)} that van den Boogaart et al. (2014)
#' include in the inner product of the Bayes space; for that convention divide
#' the result by \eqn{\sqrt{b - a}}{sqrt(b - a)} (or by
#' \eqn{\sqrt{P(\Omega)}}{sqrt(P(Omega))}).
#' 
#' @param z numeric vector, equidistant grid of points on the domain.
#' @param z_step spacing of the grid \code{z}, typically \code{diff(z[1:2])}.
#' @param f strictly positive density values on the grid: a vector of the same
#'   length as \code{z} for one density, or a matrix with one density per row and
#'   \code{length(z)} columns.
#' @param g \code{NULL}, or density values in the same format as \code{f}.
#' @param p optional vector of the same length as \code{z}: density of the
#'   reference measure \eqn{P} with respect to the Lebesgue measure. \code{NULL}
#'   (default) means the Lebesgue measure.
#' @param type \code{"rows"} (default): the distances between corresponding
#'   rows of \code{f} and \code{g} (a single number for two densities);
#'   \code{"cross"}: the matrix of distances between all rows of \code{f} and all
#'   rows of \code{g}. Ignored if \code{g} is \code{NULL}.
#' @return If \code{g} is \code{NULL}, the distances between all densities in
#'   \code{f} as an object of class \code{"dist"}. Otherwise a vector
#'   (\code{type = "rows"}) or a matrix (\code{type = "cross"}) of distances.
#' @author Matthias Templ
#' @seealso \code{\link{aDist}}, \code{\link{fcenLR}}, \code{\link{fcenLRp}},
#'   \code{\link{trapzc}}
#' @references Egozcue, J.J., Diaz-Barrero, J.L. and Pawlowsky-Glahn, V. (2006)
#' Hilbert space of probability density functions based on Aitchison geometry.
#' \emph{Acta Mathematica Sinica, English Series}, \bold{22} (4), 1175-1182.
#' \doi{10.1007/s10114-005-0678-2}
#' 
#' Egozcue, J.J. and Pawlowsky-Glahn, V. (2016) Changing the reference measure
#' in the simplex and its weighting effects. \emph{Austrian Journal of
#' Statistics}, \bold{45} (4), 25-44. \doi{10.17713/ajs.v45i4.126}
#' 
#' Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J.
#' (2020). Weighting the domain of probability densities in functional data
#' analysis. \emph{Stat}, \bold{9} (1), e283. \doi{10.1002/sta4.283}
#' 
#' van den Boogaart, K.G., Egozcue, J.J. and Pawlowsky-Glahn, V. (2014) Bayes
#' Hilbert spaces. \emph{Australian & New Zealand Journal of Statistics},
#' \bold{56} (2), 171-194. \doi{10.1111/anzs.12074}
#' @export
#' @examples
#' t <- seq(-4.7, 4.7, length = 1000)
#' t_step <- diff(t[1:2])
#' f <- dnorm(t, mean = 0, sd = 1.5)
#' g <- dnorm(t, mean = 1, sd = 1.5)
#' 
#' aDistDens(t, t_step, f, g)
#' # same value from the clr transforms
#' sqrt(trapzc(t_step, (fcenLR(t, t_step, f) - fcenLR(t, t_step, g))^2))
#' 
#' # scale invariance: densities need not be normalised
#' aDistDens(t, t_step, 10 * f, g)
#' 
#' # distance matrix of several densities (one per row)
#' dens <- t(sapply(c(0, 0.5, 1, 2), function(m) dnorm(t, mean = m, sd = 1.5)))
#' aDistDens(t, t_step, dens)
#' 
#' # with respect to a reference measure P (here a normal distribution)
#' p <- dnorm(t, sd = 2) / trapzc(t_step, dnorm(t, sd = 2))
#' aDistDens(t, t_step, f / p, g / p, p = p)
aDistDens <- function(z, z_step, f, g = NULL, p = NULL,
                      type = c("rows", "cross")) {
  type <- match.arg(type)
  nz <- length(z)
  if (nz < 2) stop("'z' needs at least two grid points")
  if (!isTRUE(all.equal(diff(z), rep(z_step, nz - 1))))
    stop("'z' must be an equidistant grid with spacing 'z_step'")
  as_dens <- function(d, arg) {
    if (is.null(d)) return(NULL)
    d <- if (is.vector(d)) matrix(d, nrow = 1) else as.matrix(d)
    if (ncol(d) != nz)
      stop("'", arg, "' must have one value per grid point of 'z'")
    if (any(!is.finite(d)) || any(d <= 0))
      stop("'", arg, "' must be strictly positive and finite on the whole grid")
    d
  }
  f <- as_dens(f, "f")
  g <- as_dens(g, "g")
  
  # trapezoidal weights, times the reference density
  w <- rep(z_step, nz)
  w[c(1, nz)] <- z_step / 2
  if (!is.null(p)) {
    if (length(p) != nz || any(!is.finite(p)) || any(p <= 0))
      stop("'p' must be a strictly positive density on the grid 'z'")
    w <- w * p
  }
  aDist(f, g, type = type, weights = w)
}
