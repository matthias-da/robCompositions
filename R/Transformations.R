#' Trapezoidal formula for numerical integration
#' 
#' @rdname trapzc
#' @name trapzc
#' @author R. Talska\email{talskarenata@seznam.cz}, K. Hron\email{karel.hron@upol.cz}
#' @description Numerical integration via trapezoidal formula.
#' @param step step of the grid
#' @param f grid evaluation of density 
#' @export
#' @return 
#' \item{\code{int}}{The value of integral computed numerically by trapezoidal formula.}
#' @examples
#' # Example (zero-integral of fcenLR density)
#' t = seq(-4.7,4.7, length = 1000)
#' t_step = diff(t[1:2])
#' mean = 0; sd = 1.5
#' f = dnorm(t, mean, sd)
#' f.fcenLR = fcenLR(t,t_step,f)
#' trapzc(t_step,f.fcenLR)
trapzc = function(step,f)
{
  return(step*(0.5*f[1]+sum(f[2:(length(f)-1)]) + 0.5*f[length(f)]))
}


#------------------------------------------------------------------------------------------------------
#' fcenLR transformation (functional)
#' 
#' @rdname fcenLR
#' @name fcenLR
#' @author R. Talska\email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron\email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description fcenLR[lambda] transformation: mapping from B^2(lambda) into L^2(lambda)
#' @param z grid of points defining the abscissa
#' @param  z_step step of the grid of the abscissa
#' @param density grid evaluation of the lambda-density
#' @export
#' @return \item{\code{out}}{grid evaluation of the lambda-density in L^2(lambda)}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020). Weighting the domain of probability densities in functional data analysis.\emph{Stat}(2020). https://doi.org/10.1002/sta4.283
#' @examples
#' # Example (normal density)
#' t = seq(-4.7,4.7, length = 1000)
#' t_step = diff(t[1:2])
#' 
#' mean = 0; sd = 1.5
#' f = dnorm(t, mean, sd)
#' f1 = f/trapzc(t_step,f)
#' 
#' f.fcenLR = fcenLR(t,t_step,f) 
#' f.fcenLRinv = fcenLRinv(t.fine,t_step,f.fcenLR)
#' 
#' plot(t,f.fcenLR, type="l",las=1, ylab="fcenLR(density)", 
#'   cex.lab=1.2,cex.axis=1.2, col="darkblue",lwd=2)
#' abline(h=0, col="red")
#' 
#' plot(t,f.fcenLRinv, type="l",las=1, 
#'   ylab="density",cex.lab=1.2,cex.axis=1.2, col="darkblue",lwd=2,lty=1)
#' lines(t,f1,lty=2,lwd=2,col="gold")   
fcenLR = function(z, z_step, density)
{
  out = log(density)-trapzc(z_step,log(density))/(max(z)-min(z))
  return(out)
}

#' fcenLRp transformation (functional)
#' 
#' @rdname fcenLRp
#' @name fcenLRp
#' @author R. Talska\email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron\email{karel.hron@upol.cz}, J.J. Egozcue, J. Palarea-Albaladejo
#' @description fcenLR[P] transformation: mapping from B2(P) into L2(P)
#' @param z grid of points defining the abscissa
#' @param z_step step of the grid of the abscissa
#' @param density grid evaluation of the P-density
#' @param p density of the reference measure P
#' @export
#' @return 
#' \item{\code{out}}{grid evaluation of the P-density in L2(P)}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020). Weighting the domain of probability densities in functional data analysis.\emph{Stat}(2020). https://doi.org/10.1002/sta4.283
fcenLRp = function(z, z_step, density, p) 
{
  P_omega = trapzc(z_step,p) # measure of the whole set (i.e. total)
  out = log(density)-trapzc(z_step,log(density)*p)/P_omega
  return(out)
}

#' fcenLRu transformation (functional)
#' 
#' @rdname fcenLRu
#' @name fcenLRu
#' @author R. Talska\email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron\email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description fcenLR[u] transformation: mapping from B2(P) into unweigted L2(lambda)
#' @param z grid of points defining the abscissa
#' @param z_step step of the grid of the abscissa
#' @param density grid evaluation of the P-density
#' @param p density of the reference measure P
#' @export
#' @return 
#' \item{\code{out}}{grid evaluation of the P-density in unweighted L2(lambda)}
#' @references Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020). Weighting the domain of probability densities in functional data analysis.\emph{Stat}(2020). https://doi.org/10.1002/sta4.283
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

#' Inverse of fcenLR transformations (functional)
#' 
#' @rdname fcenLRinv
#' @name fcenLRinv
#' @author R. Talska\email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron\email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
#' @description Inverse of fcenLR transformations
#' @details By default, it returns a unit-integral representation of density.
#' @param z grid of points defining the abscissa
#' @param z_step step of the grid of the abscissa
#' @param fcenLR grid evaluation of (i) fcenLR[lambda] transformed lambda-density,
#' (ii) fcenLR[u] transformed P-density, (iii) fcenLR[P] transformed P-density
#' @param k value of the integral of density; if k=1 it returns a unit-integral representation of density
#' @return \code{out} ... grid evaluation of (i) lambda-density in B2(lambda), 
#' (ii) P-density in unweighted B2(lambda), (iii) P-density in B2(P)
#' @export
#' @examples
#' # Example (normal density)
#' t = seq(-4.7,4.7, length = 1000)
#' t_step = diff(t[1:2])
#' 
#' mean = 0; sd = 1.5
#' f = dnorm(t, mean, sd)
#' f1 = f/trapzc(t_step,f)
#' 
#' f.fcenLR = fcenLR(t,t_step,f) 
#' f.fcenLRinv = fcenLRinv(t.fine,t_step,f.fcenLR)
#' 
#' plot(t,f.fcenLR, type="l",las=1, ylab="fcenLR(density)", 
#'   cex.lab=1.2,cex.axis=1.2, col="darkblue",lwd=2)
#' abline(h=0, col="red")
#' 
#' plot(t,f.fcenLRinv, type="l",las=1, 
#'   ylab="density",cex.lab=1.2,cex.axis=1.2, col="darkblue",lwd=2,lty=1)
#' lines(t,f1,lty=2,lwd=2,col="gold")  
fcenLRinv <- function(z, z_step, fcenLR, k=1) 
{
  return((exp(fcenLR)/trapzc(z_step,exp(fcenLR)))*k)
}

