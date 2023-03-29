#' Compositional spline
#' 
#' @rdname compositionalSpline
#' @name compositionalSpline
#' @author J. Machalova \email{jitka.machalova@upol.cz}, R. Talska \email{talskarenata@seznam.cz}
#' @description This code implements the compositional smoothing splines grounded on the theory of 
#' Bayes spaces.
#' @details The compositional splines enable to construct a spline basis in the centred logratio (clr) space of density 
#' functions (ZB-spline basis) and consequently also in the original space of densities (CB-spline basis).The resulting 
#' compositional splines in the clr space as well as the ZB-spline basis satisfy the zero integral constraint. 
#' This enables to work with compositional splines consistently in the framework of the Bayes space methodology.
#' @details Augmented knot sequence is obtained from the original knots by adding #(order-1) multiple endpoints.
#' @importFrom splines splineDesign
#' @importFrom graphics matplot matpoints
#' @param t class midpoints
#' @param clrf clr transformed values at class midpoints, i.e., fcenLR(f(t))
#' @param knots sequence of knots
#' @param w weights
#' @param order order of the spline (i.e., degree + 1)
#' @param der lth derivation
#' @param alpha smoothing parameter
#' @param spline.plot if TRUE, the resulting spline is plotted
#' @param basis.plot if TRUE, the ZB-spline basis system is plotted 
#' @return 
#' \item{\code{J}}{value of the functional J}
#' \item{\code{ZB_coef}}{ZB-spline basis coeffcients}
#' \item{\code{CV}}{score of cross-validation}
#' \item{\code{GCV}}{score of generalized cross-validation}
#' @references Machalova, J., Talska, R., Hron, K. Gaba, A. Compositional splines for representation of density functions. \emph{Comput Stat} (2020). https://doi.org/10.1007/s00180-020-01042-7
#' @export
#' @examples
#' # Example (Iris data):
#' SepalLengthCm <- iris$Sepal.Length
#' Species <- iris$Species
#' iris1 <- SepalLengthCm[iris$Species==levels(iris$Species)[1]]
#' h1 <- hist(iris1, plot = FALSE)
#' midx1 <- h1$mids
#' midy1 <- matrix(h1$density, nrow=1, ncol = length(h1$density), byrow=TRUE)
#' clrf  <- cenLR(rbind(midy1,midy1))$x.clr[1,]
#' knots <- seq(min(h1$breaks),max(h1$breaks),l=5)
#' order <- 4
#' der <- 2
#' alpha <- 0.99
#' \donttest{
#' sol1 <- compositionalSpline(t = midx1, clrf = clrf, knots = knots, 
#'   w = rep(1,length(midx1)), order = order, der = der, 
#'   alpha = alpha, spline.plot = TRUE)
#' sol1$GCV
#' ZB_coef <- sol1$ZB_coef
#' t <- seq(min(knots),max(knots),l=500)
#' t_step <- diff(t[1:2])
#' ZB_base <- ZBsplineBasis(t=t,knots,order)$ZBsplineBasis
#' sol1.t <- ZB_base%*%ZB_coef
#' sol2.t <- fcenLRinv(t,t_step,sol1.t)
#' h2 = hist(iris1,prob=TRUE,las=1)
#' points(midx1,midy1,pch=16)
#' lines(t,sol2.t,col="darkred",lwd=2)
#' # Example (normal distrubution):
#' # generate n values from normal distribution
#' set.seed(1)
#' n = 1000; mean = 0; sd = 1.5
#' raw_data = rnorm(n,mean,sd)
#'   
#' # number of classes according to Sturges rule
#' n.class = round(1+1.43*log(n),0)
#'   
#' # Interval midpoints
#' parnition = seq(-5,5,length=(n.class+1))
#' t.mid = c(); for (i in 1:n.class){t.mid[i]=(parnition[i+1]+parnition[i])/2}
#'   
#' counts = table(cut(raw_data,parnition))
#' prob = counts/sum(counts)                # probabilities
#' dens.raw = prob/diff(parnition)          # raw density data
#' clrf =  cenLR(rbind(dens.raw,dens.raw))$x.clr[1,]  # raw clr density data
#'   
#' # set the input parameters for smoothing 
#' knots = seq(min(parnition),max(parnition),l=5)
#' w = rep(1,length(clrf))
#' order = 4
#' der = 2
#' alpha = 0.5
#' spline = compositionalSpline(t = t.mid, clrf = clrf, knots = knots, 
#'   w = w, order = order, der = der, alpha = alpha, 
#'   spline.plot=TRUE, basis.plot=FALSE)
#'   
#' # ZB-spline coefficients
#' ZB_coef = spline$ZB_coef
#'   
#' # ZB-spline basis evaluated on the grid "t.fine"
#' t.fine = seq(min(knots),max(knots),l=1000)
#' ZB_base = ZBsplineBasis(t=t.fine,knots,order)$ZBsplineBasis
#'   
#' # Compositional spline in the clr space (evaluated on the grid t.fine)
#' comp.spline.clr = ZB_base%*%ZB_coef
#'   
#' # Compositional spline in the Bayes space (evaluated on the grid t.fine)
#' comp.spline = fcenLRinv(t.fine,diff(t.fine)[1:2],comp.spline.clr)
#'   
#' # Unit-integral representation of truncated true normal density function 
#' dens.true = dnorm(t.fine, mean, sd)/trapzc(diff(t.fine)[1:2],dnorm(t.fine, mean, sd))
#'   
#' # Plot of compositional spline together with raw density data
#' matplot(t.fine,comp.spline,type="l",
#'     lty=1, las=1, col="darkblue", xlab="t", 
#'     ylab="density",lwd=2,cex.axis=1.2,cex.lab=1.2,ylim=c(0,0.28))
#' matpoints(t.mid,dens.raw,pch = 8, col="darkblue", cex=1.3)
#'   
#' # Add true normal density function
#' matlines(t.fine,dens.true,col="darkred",lwd=2)
#' }
compositionalSpline = function(t,clrf,knots,w,order,der,alpha,spline.plot = FALSE,basis.plot=FALSE){
  ## Comments from Matthias:
  # 1. Why not giving as first argument some data, and do the fcenLR(f(t)) within compositionalSpline()?
  # This would be more userfriendly.
  # 2. Please provide executable examples for the users.
  # 3. I put all helper functions within compositionalSpline, but can 
  # include it as usual documented functions in robCompositions as well. The disadvantage 
  # to do so is that more and more documented functions are available, but these functions are only used internally.
  # 4. I will add a comment like plot = FALSE, so that the plot is only done when the user decides for it.
  # Alternatively I also can implement a plot method.
  # 5. Which parameter values can be set to a sensible default value? E.g. order = 2

  ## Comments from Renata
  # ad 1. The order of arguments was changed. We prefer to let the input data as clr.
  # ad 2. The example based on iris1 data was expanded and one related to normal distribution was considered.
  # ad 3. To run the code, only the function trapzc() is needed. 
  #       Functions trapzc(), fcenLR(), fcenLRp(), fcenLRu(), fcenLRinv() are to be included in robCompositions.
  # ad 4. Two more additional input paramaters were added to the function - spline.plot, basis.plot.
  # ad 5. It is not necessary.
    
  # Verification for parameter alpha
  if (alpha<=0 | alpha>1) stop ("parameter alpha must be from interval (0,1]")
  
  k = order
  r = length(knots) 

  Celkova_Delka = 2*(k-1) + r 
  lambda = c() 
  for (i in 1:(Celkova_Delka)){
    if (i <= k-1){lambda[i] = knots[1]}
    if ((i > k-1) && (i <= r + k-1)){lambda[i] = knots[i-(k-1)]}
    if (i > r+ k-1){lambda[i] = knots[r]}
  }
  
  # Collocation matrix B
  B = splineDesign(lambda, t, k, outer.ok = TRUE)
  
  # Diag matrix with weights
  W = diag(w)
  
  # Collocation matrix B (:=BB) evaluated on finner grid
  parnition = seq(min(lambda), max(lambda), length = 1000)    
  lambda_index = c(0:(r-1)) 
  g = lambda_index[length(lambda_index) - 1]
  # Dimension(space of splines)
  N = g+(k-1)+1
  BB = array(0, c(length(parnition),N))
  l = c()
  for(i in (1:N)){
    for (j in 1:(k+1)){
      l[j] = lambda[i+j-1]
    }
    BB[ ,i] = splineDesign(l, parnition, k, outer.ok = TRUE) 
  }
  # Verification of full column rank of collocation matrix K
  if (length(t) <= N) stop ('length(t) must be higher then Dimension(space of splines)')
  if (qr(B)$rank != N) stop ('Collocaton matrix does not have full column rank.')
  
   # Matrix S
  S = array(0)
  if (der == 0){
    S = diag(1, c(N,N))
  }
  if (der >= (k-1)) {stop ('Error. The lth derivation is not from the set {1,2,...,order-2}.')
  } else {
    S_pom = diag(1,N,N)
    for (j in 1:der)
    {
      D_mat = array(0)
      rozdil = lambda[(1+k):(N+k-j)] - lambda[(1+j):(N)]
      D_mat = (k-j)*diag(1/rozdil)
      L_mat = array(0, c(N-j,N-j+1))
      for (J in (1:(N-j))){
        L_mat[J,J] = (-1)
        L_mat[J,J+1] = 1
      }
      S_pom = D_mat%*%L_mat%*%S_pom
    }
    S = S_pom
  }
  
  # Matrix M: order of spline = k-der
  kk = k-der 
  
  # Matrix M: augmented knot sequence
  celkova_delka = 2*(kk-1) + r                                         
  Lambda = c()
  for (i in 1:celkova_delka){
    if (i <= (kk-1)){Lambda[i] = knots[1]}
    if ((i > kk-1) && (i <= r + kk-1)){Lambda[i] = knots[i-(kk-1)]}
    if (i > (r+(kk-1))){Lambda[i] = knots[r]}
  } 
  Parnition = seq(min(Lambda), max(Lambda),  length = 10000)    
  Lambda_index = c(0:(r-1)) 
  G = Lambda_index[length(Lambda_index) - 1]
  
  # Matrix M: spline space dimension
  NN = G+(kk-1)+1
  
  # Matrix M: collocation matrix BBB
  BBB = splineDesign(Lambda, Parnition, kk, outer.ok=TRUE)
  
  # Matrix M: function for computing integral
  step=diff(Parnition[1:2])
  
  # Matrix M
  M=array(0, c(NN,NN))
  for (i in 1:NN){
    for (j in 1:NN){
      nenulove = c()
      soucin = BBB[,i]*BBB[,j]
      for (m in 1:length(Parnition)){
        if (soucin[m] != 0) {nenulove[m] = soucin[m]}
      }
      M[i,j]=trapzc(step, soucin)
    }
  }
  
  # Matrix D
  difference = lambda[(1+k):(r+2*(k-1))] - lambda[(1:(r+k-2))]
  D = (k)*diag(1/difference)
  
  # Matrix K
  K = array(0, c(N,N-1))
  K[1,1]=1
  K[N,N-1]=-1
  
  for (j in (2:(N-1))){
    K[j,j-1] = (-1)
    K[j,j] = 1
  }

  # Matrix U
  U = D%*%K

  # Matrix G, vector g
  GG = t(U)%*%((1-alpha)*t(S)%*%M%*%S + alpha*t(B)%*%W%*%B)%*%U
  gg = alpha*t(K)%*%t(D)%*%t(B)%*%W%*%clrf
  
  # vector of B-spline coefficients := z
  z = solve(GG)%*%gg
  
  # ZB-spline basis
  Zbasis = B%*%D%*%K
  
  # ZB-spline basis (evaluated on a grid finner grid)
  Zbasis_finner = BB%*%D%*%K
  
  if (basis.plot == TRUE){
    matplot(parnition,Zbasis_finner, type="l",lty=1,las=1, xlab="t", ylab="fcenLR(density)",col = rainbow(dim(Zbasis_finner)[2])
            ,main="ZB-spline basis")
    abline(v=knots, col="gray",lty=2)
  }
  
  # Resulting compositional spline in L20
  spline0 = Zbasis_finner%*%z
  
  if (spline.plot==TRUE) {
    matplot(parnition,spline0, type="l",las=1,xlab="t",ylab="fcenLR(density)",col="darkblue",lwd=2,cex.lab=1.2,cex.axis=1.2,
            ylim = c(min(c(min(clrf),min(spline0))),max(c(max(clrf),max(spline0)))),
            main = paste("Compositional spline of degree k =",k-1))
    matpoints(t,clrf, pch = 8, col="darkblue", cex=1.3)
    abline(h=0,col="red",lty=2,lwd=1)
    abline(v=knots,col="gray",lty=2,lwd=1)
  }
  
  # CV
  clrf = as.matrix(clrf)
  Hmat = (B%*%D%*%K)%*%solve(GG)%*%(alpha*t(K)%*%t(D)%*%t(B)%*%W)
  clrfhat = (B%*%D%*%K)%*%z
  reziduals = (clrf-clrfhat)
  Hmat_diag = c()
  for (i in 1:length(clrf))  Hmat_diag[i] = Hmat[i,i]
  
  Hmat_diag_mean = (1/length(clrf))*sum(Hmat_diag)
  
  CV = (1/length(clrf))*sum((reziduals/(rep(1,length(Hmat_diag))-Hmat_diag))^2)
  GCV = (1/length(clrf))*(sum((reziduals)^2))/((1-Hmat_diag_mean)^2)
  
  J = (1-alpha)*t(z)%*%t(U)%*%t(S)%*%M%*%S%*%U%*%z + alpha*t(clrf-B%*%D%*%K%*%z)%*%W%*%(clrf-B%*%D%*%K%*%z)
  return(list(J=J, ZB_coef=z, CV=CV, GCV=GCV))
}







