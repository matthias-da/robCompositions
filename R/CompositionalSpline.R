#' compositional spline
#' 
#' @rdname compositionalSpline
#' @name compositionalSpline
#' @param 
#' @author J. Machalova \email{jitka.machalova@upol.cz}, R. Talska \email{talskarenata@seznam.cz}, K. Hron, A. Gaba
#' @description This code implements the compositional smooting splines grounded on the theory of 
#' Bayes spaces.
#' @details Augmented knot sequence is obtained from the original knots by adding #(order-1) multiple endpoints.
#' @importFrom splines splineDesign
#' @importFrom graphics matplot matpoints
#' @param knots sequence of knots
#' @param t class midpoints
#' @param clrf clr transformed values at class midpoints, i.e., clr(f(t))
#' @param w weights
#' @param order order of the spline (i.e., degree + 1)
#' @param der lth derivation
#' @param alpha smoothing parameter
#' @param plot if TRUE, thre resulting spline is plotted
#' @return 
#' \item{\code{J}}{value of the functional J}
#' \item{\code{ZB_coef}}{ZB-spline basis coeffcients}
#' \item{\code{CV}}{score of cross-validation}
#' \item{\code{GCV}}{score of generalized cross-validation}
#' @references Machalova, J., Talska, R., Hron, K. Gaba, A. Compositional splines for representation of density functions. \emph{Comput Stat} (2020). https://doi.org/10.1007/s00180-020-01042-7
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
#' sol1 <- compositionalSpline(knots = 3, t = 2, clrf = , w =, order =, der = , alpha =)
#' }
compositionalSpline = function(knots,t,clrf,w,order,der,alpha,plot = FALSE){
  ## Comments from Matthias:
  # 1. Why not giving as first argument some data, and do the clr(f(t)) within compositionalSpline()?
  # This would be more userfriendly.
  # 2. Please provide executable examples for the users.
  # 3. I put all helper functions within compositionalSpline, but can 
  # include it as usual documented functions in robCompositions as well. The disadvantage 
  # to do so is that more and more documented functions are available, but these functions are only used internally.
  # 4. I will add a comment like plot = FALSE, so that the plot is only done when the user decides for it.
  # Alternatively I also can implement a plot method.
  # 5. Which parameter values can be set to a sensible default value? E.g. order = 2
  
  # ---- LOAD FUNCTIONS-------------------------------------------------------------------------------------------
  # Numerical integration via trapezoidal formula
  # Input: c = grid evaluation of the function
  #        step = step of the grid
  SLP=function(step, c){
    integral = step*(0.5*c[1]+sum(c[2:(length(c)-1)]) +0.5*c[length(c)])
    return (integral)
  }
  # Numerical integration via trapezoidal formula
  # Input: f = grid evaluation of the function
  #        z_step = step of the grid
  trapzc = function(step,f) 
  {
    int = step*(0.5*f[1]+sum(f[2:(length(f)-1)]) + 0.5*f[length(f)])
    return (int)
  }
  
  # clr[lambda] transformation: mapping from B2(lambda) into L2(lambda)
  # Input: z = grid of point defining the abscissa 
  #        z_step = step of the grid of the abscissa
  #        density = grid evaluation of the lambda-density
  # Output: grid evaluation of the lambda-density in L2(lambda)
  
  clr = function(z, z_step, density)
  {
    return(log(density)-trapzc(z_step,log(density))/(max(z)-min(z)))
  }
  
  # clr[P] transformation: mapping from B2(P) into L2(P)
  # Input: z = grid of point defining the abscissa 
  #        z_step = step of the grid of the abscissa
  #        density = grid evaluation of the P-density
  #        p = density of reference measure P
  #        P_omega = measure of the whole set (i.e. total)
  # Output: grid evaluation of the P-density in L2(P)
  clrp = function(z, z_step, density, p, P_omega) 
  {
    return(log(density)-trapzc(z_step,log(density)*p)/P_omega)
  }
  
  # clr[u] transformation: mapping from B2(P) into unweigted L2(lambda)
  # Input: z = grid of point defining the abscissa 
  #        z_step = step of the grid of the abscissa
  #        density = grid evaluation of the P-density
  #        p = density of reference measure P
  #        P_omega = measure of the whole set (i.e. total)
  # Output: grid evaluation of the P-density in inweighted L2(lambda)
  clru = function(z, z_step, density, p, P_omega) 
  {
    return(sqrt(p)*(log(density)-trapzc(z_step,log(density)*p)/P_omega))
  }
  
  # Inverse of clr transformation
  # Input: z = grid of point defining the abscissa 
  #        z_step = step of the grid of the abscissa
  #        clr = grid evaluation of (i)   clr[lambda] transformed lambda-density
  #                                 (ii)  clr[u] transformed P-density
  #                                 (iii) clr[P] transformed P-density
  # Output: grid evaluation of (i)   lambda-density in B2(lambda)
  #                            (ii)  P-density in unweighted B2(lambda)
  #                            (iii) P-density in B2(P)
  # By default, it returns a unit-integral representation of density.
  clr2density = function(z, z_step, clr, k=1) 
  {
    return((exp(clr)/trapzc(z_step,exp(clr)))*k)
  }
 
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
  if (der >= (k-1)) {stop ('Error. the lth derivation is not from the set {1,2,...,order-2}')
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
      M[i,j]=SLP(step, soucin)
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
  
  # CB-spline basis
  Zbasis = B%*%D%*%K
  
  #matplot(parnition,Zbasis, type="l",lty=1,las=1, xlab="t", col = rainbow(dim(Bbasis)[2]))
  #abline(v=knots, col="gray",lty=2)
  
  # Resulting compositional spline in L20
  spline0 = (BB%*%D%*%K)%*%z
  
  if(plot){
    matplot(parnition,spline0, type="l",las=1,xlab="t",ylab="clr(density)",col="darkblue",lwd=2,cex.lab=1.2,cex.axis=1.2,
            ylim = c(min(c(min(clrf),min(spline0))),max(c(max(clrf),max(spline0)))),
            main = paste("Compositional spline of degree k =",k-1))
    matpoints(t,clrf, pch = 8, col="darkblue", cex=1.3)
    abline(h=0,col="red",lty=2,lwd=1)
  }
  
  # CV
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

