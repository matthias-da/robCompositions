#' ZB-spline basis
#' 
#' @rdname ZBsplineBasis
#' @name ZBsplineBasis
#' @author J. Machalova \email{jitka.machalova@upol.cz}, R. Talska \email{talskarenata@seznam.cz}
#' @description Spline basis system having zero-integral on I=[a,b] of the L^2_0 space (called ZB-splines) has been
#' proposed for an basis representation of fcenLR transformed probability density functions. The ZB-spline basis functions can be back
#' transformed to Bayes spaces using inverse of fcenLR transformation, resulting in compositional B-splines (CB-splines), 
#' and forming a basis system of the Bayes spaces. 
#' @importFrom fda create.bspline.basis eval.basis
#' @param t a vector of argument values at which the ZB-spline basis functions are to be evaluated
#' @param knots sequence of knots
#' @param order order of the ZB-splines (i.e., degree + 1)
#' @param basis.plot if TRUE, the ZB-spline basis system is plotted
#' @importFrom grDevices rainbow
#' @return 
#' \item{\code{ZBsplineBasis}}{matrix of ZB-spline basis functions evaluated at a vector of argument values t}
#' \item{\code{nbasis}}{number of ZB-spline basis functions}
#' @references Machalova, J., Talska, R., Hron, K. Gaba, A. Compositional splines 
#' for representation of density functions. \emph{Comput Stat} (2020). 
#' https://doi.org/10.1007/s00180-020-01042-7
#' @export
#' @examples
#' # Example: ZB-spline basis functions evaluated at a vector of argument values t
#' t = seq(0,20,l=500)
#' knots = c(0,2,5,9,14,20)
#' order = 4
#' 
#' ZBsplineBasis.out = ZBsplineBasis(t,knots,order, basis.plot=TRUE)
#' 
#' # Back-transformation of ZB-spline basis functions from L^2_0 to Bayes space -> 
#' # CB-spline basis functions
#' CBsplineBasis=NULL
#' for (i in 1:ZBsplineBasis.out$nbasis)
#' {
#'  CB_spline = fcenLRinv(t,diff(t)[1:2],ZBsplineBasis.out$ZBsplineBasis[,i])
#'  CBsplineBasis = cbind(CBsplineBasis,CB_spline)
#' }
#' 
#' matplot(t,CBsplineBasis, type="l",lty=1, las=1, 
#'   col=rainbow(ZBsplineBasis.out$nbasis), xlab="t", 
#'   ylab="CB-spline basis",
#' cex.lab=1.2,cex.axis=1.2)
#' abline(v=knots, col="gray", lty=2)
ZBsplineBasis = function(t,knots,order,basis.plot=FALSE){
  # Verification
  if (t[1] != knots[1] | t[length(t)] != knots[length(knots)]) stop ("Knots do not span the values of t.")
  
  k = order
  r = length(knots)
  lambda_index = c(0:(r-1)) 
  g = lambda_index[length(lambda_index) - 1]
  N = g+(k-1)+1
  lambda = c(rep(min(knots),k-1),knots,rep(max(knots),k-1))
  
  # standard B-spline basis; collocation matrix := C
  splajn.basis = create.bspline.basis(range(knots),nbasis = N , norder = k, breaks = knots)
  C = eval.basis(t, splajn.basis)
  
  # Matrix D
  difference = lambda[(1+k):(r+2*(k-1))] - lambda[(1:(r+k-2))]
  D = (k)*diag(1/difference)
  
  # Matrix L
  L = array(0, c(N,N-1))
  L[1,1]=1
  L[N,N-1]=-1
  
  for (j in (2:(N-1))){
    L[j,j-1] = (-1)
    L[j,j] = 1
  }
  
  # ZB-spline basis; collocation matrix := C0
  C0 = C%*%D%*%L
  
  if (basis.plot==TRUE){
    matplot(t,C0, type="l",lty=1, las=1, col=rainbow(N-1), xlab="t", ylab="ZB-spline basis",cex.lab=1.2,cex.axis=1.2)
    abline(v=knots, col="gray", lty=2)
  }
  
  nbasis = N-1
  return(list(ZBsplineBasis=C0, nbasis=nbasis))
}
