#' Trapezoidal rule for numerical integration on an equidistant grid
#'
#' @author R. Talska \email{talskarenata@seznam.cz}, K. Hron \email{karel.hron@upol.cz}
#' @description Approximates the integral of a function from its values on an
#' equidistant grid by the composite trapezoidal rule,
#' \deqn{\int_a^b f(t)\,dt \approx h\left(\frac{f_1}{2} + f_2 + \dots + f_{n-1} + \frac{f_n}{2}\right),}{integral f(t) dt ~ h * (f[1]/2 + f[2] + ... + f[n-1] + f[n]/2),}
#' where \eqn{h} is the grid spacing. It is the integration routine used by
#' \code{\link{fcenLR}}, \code{\link{fcenLRp}}, \code{\link{fcenLRu}},
#' \code{\link{fcenLRinv}} and \code{\link{compositionalSpline}}.
#' @param step spacing \eqn{h} of the (equidistant) grid.
#' @param f numeric vector of at least two function values on the grid, e.g. a
#' density or a clr-transformed density.
#' @return A single number: the approximate value of the integral.
#' @seealso \code{\link{fcenLR}}, \code{\link{fcenLRinv}}
#' @export
#' @examples
#' t <- seq(-4.7, 4.7, length = 1000)
#' t_step <- diff(t[1:2])
#' f <- dnorm(t, mean = 0, sd = 1.5)
#'
#' # integral of the density over the grid (close to 1)
#' trapzc(t_step, f)
#'
#' # the clr transform of a density integrates to zero
#' trapzc(t_step, fcenLR(t, t_step, f))
trapzc <- function(step, f)
{
  n <- length(f)
  if (n < 2) stop("'f' needs at least two grid points")
  step * (sum(f) - 0.5 * (f[1] + f[n]))
}


#------------------------------------------------------------------------------------------------------
#' Functional centred log-ratio (clr) transformation of a density
#'
#' @author R. Talska \email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron \email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description Maps a density from the Bayes space \eqn{B^2(\lambda)} (densities
#' with respect to the Lebesgue measure \eqn{\lambda} on a bounded interval
#' \eqn{[a, b]}) to the space \eqn{L^2(\lambda)} of square-integrable functions,
#' \deqn{\mathrm{fcenLR}_\lambda(f)(z) = \log f(z) - \frac{1}{b-a}\int_a^b \log f(t)\,dt.}{fcenLR(f)(z) = log f(z) - 1/(b-a) * integral_a^b log f(t) dt.}
#' This is the functional counterpart of the clr transformation (\code{\link{cenLR}})
#' for compositions. The result integrates to zero over \eqn{[a, b]}, so densities
#' can then be analysed with standard methods of functional data analysis.
#' @details The integral is approximated by the trapezoidal rule (\code{\link{trapzc}})
#' with spacing \code{z_step}, so \code{z} must be an equidistant grid with that
#' spacing; the domain length \eqn{b-a} is taken as \code{max(z) - min(z)}.
#'
#' Densities are scale invariant in the Bayes space: \code{density} does not need
#' to integrate to one, and \code{fcenLR(z, z_step, c * density)} equals
#' \code{fcenLR(z, z_step, density)} for any \eqn{c > 0}. All values of
#' \code{density} must be strictly positive; a zero anywhere on the grid makes the
#' whole result \code{NaN}.
#'
#' Use \code{\link{fcenLRinv}} for the back-transformation, and \code{\link{fcenLRp}}
#' or \code{\link{fcenLRu}} for densities with respect to a reference measure
#' \eqn{P} other than the Lebesgue measure.
#' @param z numeric vector, equidistant grid of points on the domain \eqn{[a, b]}.
#' @param z_step spacing of the grid \code{z}, typically \code{diff(z[1:2])}.
#' @param density numeric vector of the same length as \code{z}: strictly positive
#' values of the density on the grid.
#' @return A numeric vector of the same length as \code{density}: the clr
#' transform evaluated on the grid. It integrates to zero over the domain.
#' @seealso \code{\link{fcenLRinv}}, \code{\link{fcenLRp}}, \code{\link{fcenLRu}},
#' \code{\link{trapzc}}, \code{\link{cenLR}}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020).
#' Weighting the domain of probability densities in functional data analysis.
#' \emph{Stat}. \doi{10.1002/sta4.283}
#' @export
#' @examples
#' # normal density on a grid
#' t <- seq(-4.7, 4.7, length = 1000)
#' t_step <- diff(t[1:2])
#' f <- dnorm(t, mean = 0, sd = 1.5)
#'
#' f.fcenLR <- fcenLR(t, t_step, f)
#' trapzc(t_step, f.fcenLR)          # zero integral
#'
#' # the back-transformation returns the unit-integral version of f
#' f.back <- fcenLRinv(t, t_step, f.fcenLR)
#' f1 <- f / trapzc(t_step, f)
#' all.equal(f.back, f1)
#'
#' plot(t, f.fcenLR, type = "l", las = 1, ylab = "fcenLR(density)",
#'   col = "darkblue", lwd = 2)
#' abline(h = 0, col = "red")
#'
#' plot(t, f.back, type = "l", las = 1, ylab = "density",
#'   col = "darkblue", lwd = 2)
#' lines(t, f1, lty = 2, lwd = 2, col = "gold")
fcenLR = function(z, z_step, density)
{
  out = log(density)-trapzc(z_step,log(density))/(max(z)-min(z))
  return(out)
}

#' Functional clr transformation of a density with respect to a reference measure P
#'
#' @author R. Talska \email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron \email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description Maps a \eqn{P}-density from the weighted Bayes space \eqn{B^2(P)}
#' to the weighted space \eqn{L^2(P)}, where the reference measure \eqn{P} has
#' density \eqn{p} with respect to the Lebesgue measure,
#' \deqn{\mathrm{fcenLR}_P(f)(z) = \log f(z) - \frac{1}{P(\Omega)}\int_\Omega \log f(t)\,p(t)\,dt.}{fcenLR_P(f)(z) = log f(z) - 1/P(Omega) * integral log f(t) p(t) dt.}
#' With \eqn{p} constant this reduces to \code{\link{fcenLR}}.
#' @details Integrals are approximated by \code{\link{trapzc}}, so \code{z} must be
#' an equidistant grid with spacing \code{z_step}. All values of \code{density}
#' must be strictly positive. The result is centred with respect to \eqn{P}: the
#' integral of \code{fcenLRp(...) * p} is zero. A worked example covering
#' \code{fcenLR}, \code{fcenLRp} and \code{fcenLRu} is given in \code{\link{fcenLRu}}.
#' @param z numeric vector, equidistant grid of points on the domain.
#' @param z_step spacing of the grid \code{z}.
#' @param density numeric vector of the same length as \code{z}: strictly positive
#' values of the \eqn{P}-density on the grid.
#' @param p numeric vector of the same length as \code{z}: density of the reference
#' measure \eqn{P} with respect to the Lebesgue measure.
#' @return A numeric vector of the same length as \code{density}: the
#' \eqn{P}-weighted clr transform evaluated on the grid (an element of \eqn{L^2(P)}).
#' @seealso \code{\link{fcenLR}}, \code{\link{fcenLRu}}, \code{\link{fcenLRinv}},
#' \code{\link{trapzc}}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020).
#' Weighting the domain of probability densities in functional data analysis.
#' \emph{Stat}. \doi{10.1002/sta4.283}
#' @export
fcenLRp = function(z, z_step, density, p) 
{
  P_omega = trapzc(z_step,p) # measure of the whole set (i.e. total)
  out = log(density)-trapzc(z_step,log(density)*p)/P_omega
  return(out)
}

#' Unweighted functional clr transformation of a density with respect to a reference measure P
#'
#' @author R. Talska \email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron \email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description Maps a \eqn{P}-density from the weighted Bayes space \eqn{B^2(P)}
#' to the unweighted space \eqn{L^2(\lambda)} by multiplying the
#' \eqn{P}-weighted clr transform (\code{\link{fcenLRp}}) by \eqn{\sqrt{p}},
#' \deqn{\mathrm{fcenLR}_u(f)(z) = \sqrt{p(z)}\;\mathrm{fcenLR}_P(f)(z).}{fcenLR_u(f)(z) = sqrt(p(z)) * fcenLR_P(f)(z).}
#' This moves the weighting from the measure into the function, so that methods
#' for unweighted \eqn{L^2} data (e.g. functional PCA) can be applied.
#' @details Integrals are approximated by \code{\link{trapzc}}, so \code{z} must be
#' an equidistant grid with spacing \code{z_step}. All values of \code{density}
#' must be strictly positive. Back-transforming the result with
#' \code{\link{fcenLRinv}} gives the representation of the density in the
#' unweighted Bayes space \eqn{B^2(\lambda)}.
#' @param z numeric vector, equidistant grid of points on the domain.
#' @param z_step spacing of the grid \code{z}.
#' @param density numeric vector of the same length as \code{z}: strictly positive
#' values of the \eqn{P}-density on the grid.
#' @param p numeric vector of the same length as \code{z}: density of the reference
#' measure \eqn{P} with respect to the Lebesgue measure.
#' @return A numeric vector of the same length as \code{density}: the unweighted
#' clr transform evaluated on the grid (an element of \eqn{L^2(\lambda)}).
#' @seealso \code{\link{fcenLR}}, \code{\link{fcenLRp}}, \code{\link{fcenLRinv}},
#' \code{\link{trapzc}}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020).
#' Weighting the domain of probability densities in functional data analysis.
#' \emph{Stat}. \doi{10.1002/sta4.283}
#' @export
#' @examples
#' # Common example for all transformations - fcenLR, fcenLRp, fcenLRu 
#' # Example (log normal distribution under the reference P) 
#' t = seq(1,10, length = 1000)
#' t_step = diff(t[1:2])
#' 
#' # Log normal density w.r.t. Lebesgue reference measure in B2(lambda)
#' f = dlnorm(t, meanlog = 1.5, sdlog = 0.5)
#' 
#' # Log normal density w.r.t. Lebesgue reference measure in L2(lambda)
#' f.fcenLR = fcenLR(t,t_step,f) 
#' 
#' # New reference given by exponential density
#' p = dexp(t,0.25)/trapzc(t_step,dexp(t,0.25))
#' 
#' # Plot of log normal density w.r.t. Lebesgue reference measure 
#' # in B2(lambda) together with the new reference density p
#' matplot(t,f,type="l",las=1, ylab="density",cex.lab=1.2,cex.axis=1.2, 
#'   col="black",lwd=2,ylim=c(0,0.3),xlab="t")
#' matlines(t,p,col="blue")
#' text(2,0.25,"p",col="blue")
#' text(4,0.22,"f",col="black")
#' 
#' # Log-normal density w.r.t. exponential distribution in B2(P) 
#' # (unit-integral representation)
#' fp = (f/p)/trapzc(t_step,f/p)
#' 
#' # Log-normal density w.r.t. exponential distribution in L2(P)
#' fp.fcenLRp = fcenLRp(t,t_step,fp,p)
#' 
#' # Log-normal density w.r.t. exponential distribution in L2(lambda)
#' fp.fcenLRu = fcenLRu(t,t_step,fp,p)
#' 
#' # Log-normal density w.r.t. exponential distribution in B2(lambda)
#' fp.u = fcenLRinv(t,t_step,fp.fcenLRu)
#' 
#' # Plot
#' layout(rbind(c(1,2,3,4),c(7,8,5,6)))
#' par(cex=1.1)
#' 
#' plot(t, f.fcenLR, type='l', ylab=expression(fcenLR[lambda](f)), 
#'   xlab='t',las=1,ylim=c(-3,3),
#'   main=expression(bold(atop(paste('(a) Representation of f in ', L^2, (lambda)),'[not weighted]'))))
#' abline(h=0,col="red")
#' 
#' plot(t, f, type='l', ylab=expression(f[lambda]), 
#'   xlab='t',las=1,ylim=c(0,0.4),
#'   main=expression(bold(atop(paste('(b) Density f in ', B^2, (lambda)),'[not weighted]'))))
#' 
#' plot(t, fp, type='l', ylab=expression(f[P]), xlab='t',
#'   las=1,ylim=c(0,0.4),
#'   main=expression(bold(atop(paste('(c) Density f in ', B^2, (P)),'[weighted with P]'))))
#' 
#' plot(t, fp.fcenLRp, type='l', ylab=expression(fcenLR[P](f[P])), 
#'   xlab='t',las=1,ylim=c(-3,3), 
#'   main=expression(bold(atop(paste('(d) Representation of f in ', L^2, (P)),'[weighted with P]'))))
#' abline(h=0,col="red")
#' 
#' plot(t, fp.u, type='l', ylab=expression(paste(omega^(-1),(f[P]))), 
#'   xlab='t',las=1,ylim=c(0,0.4), 
#'   main=expression(bold(atop(paste('(e) Representation of f in ', B^2, (lambda)),'[unweighted]'))))
#' 
#' plot(t, fp.fcenLRu, type='l', ylab=expression(paste(fcenLR[u](f[P]))), 
#'   xlab='t',las=1,ylim=c(-3,3),
#'   main=expression(bold(atop(paste('(f) Representation of f in ', L^2, (lambda)),'[unweighted]'))))
#' abline(h=0,col="red")
fcenLRu = function(z, z_step, density, p) 
{
  P_omega = trapzc(z_step,p) # measure of the whole set (i.e. total)
  out = sqrt(p)*(log(density)-trapzc(z_step,log(density)*p)/P_omega)
  return(out)
}

#' Inverse of the functional clr transformations
#'
#' @author R. Talska \email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron \email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description Maps a clr-transformed function back to a density,
#' \deqn{f(z) = k\,\frac{\exp(g(z))}{\int \exp(g(t))\,dt},}{f(z) = k * exp(g(z)) / integral exp(g(t)) dt,}
#' where \eqn{g} is the output of \code{\link{fcenLR}}, \code{\link{fcenLRp}} or
#' \code{\link{fcenLRu}}. Because densities in the Bayes space are scale invariant,
#' the result is only defined up to a constant; \code{k} fixes it so that the
#' returned density integrates to \code{k}.
#' @details The integral is approximated by \code{\link{trapzc}} with spacing
#' \code{z_step}. Depending on the input, the result is (i) the
#' \eqn{\lambda}-density in \eqn{B^2(\lambda)} for \code{fcenLR} output, (ii) the
#' \eqn{P}-density in \eqn{B^2(P)} for \code{fcenLRp} output, or (iii) the
#' representation of the \eqn{P}-density in the unweighted space
#' \eqn{B^2(\lambda)} for \code{fcenLRu} output.
#' @param z numeric vector, the grid on which \code{fcenLR} is evaluated. It is
#' currently not used in the computation and kept for consistency with the other
#' functions of the family.
#' @param z_step spacing of the (equidistant) grid.
#' @param fcenLR numeric vector, a clr-transformed density on the grid, as returned
#' by \code{\link{fcenLR}}, \code{\link{fcenLRp}} or \code{\link{fcenLRu}}.
#' @param k positive number, the value the returned density should integrate to;
#' the default \code{k = 1} gives a probability density.
#' @return A numeric vector of the same length as \code{fcenLR}: the density
#' evaluated on the grid, integrating to \code{k}.
#' @seealso \code{\link{fcenLR}}, \code{\link{fcenLRp}}, \code{\link{fcenLRu}},
#' \code{\link{trapzc}}, \code{\link{cenLRinv}}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020).
#' Weighting the domain of probability densities in functional data analysis.
#' \emph{Stat}. \doi{10.1002/sta4.283}
#' @export
#' @examples
#' t <- seq(-4.7, 4.7, length = 1000)
#' t_step <- diff(t[1:2])
#' f <- dnorm(t, mean = 0, sd = 1.5)
#'
#' f.fcenLR <- fcenLR(t, t_step, f)
#'
#' # back to a unit-integral density ...
#' f.back <- fcenLRinv(t, t_step, f.fcenLR)
#' trapzc(t_step, f.back)
#'
#' # ... or to a density integrating to 100
#' trapzc(t_step, fcenLRinv(t, t_step, f.fcenLR, k = 100))
#'
#' plot(t, f.back, type = "l", las = 1, ylab = "density",
#'   col = "darkblue", lwd = 2)
fcenLRinv <- function(z, z_step, fcenLR, k=1) 
{
  return((exp(fcenLR)/trapzc(z_step,exp(fcenLR)))*k)
}

