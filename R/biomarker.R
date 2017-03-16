#' biomarker
#' 
#' Robust biomarker identification and outlier diagnostics
#' 
#' @name biomarker
#' @author Jan Walach
#' @aliases biomarker print.biomarker summary.biomarker
#' @param x data frame
#' @param cut cut-off value, initialy set as 0.975 quantile of standard normal distribution
#' @param g1 vector with locations of observations of group 1
#' @param g2 vector with locations of observations of group 2
#' @param type type of estimation of the variation matrix. Possible values are \code{"sd"},  \code{"mad"} and \code{"tau"}, representing Standard deviation, Median absolute deviation and Tau estimator of scale
#' @param diag logical, indicating wheter outlier diagnostic should be computed
#' @param plot logical, indicating wheter Vstar values should be plotted
#' @param diag.plot logical, indicating wheter outlier diagnostic plot should be made
#' @description The function for identification of biomakers and 
#' outlier diagnostics as described in paper "Robust biomarker 
#' identification in a two-class problem based on pairwise log-ratios"
#' @author Jan Walach 
#' @details The method computes variation matrices separately with 
#' observations from both groups and also together with all observations. 
#' Then, \emph{V} statistics is then computed and normalized. 
#' The variables, for which according \emph{V*} values are bigger that the 
#' cut-off value are considered as biomarkers.
#' @seealso \link{plot.biomarker}
#' @return The function returns object of type "biomarker".
#' Functions \code{print}, \code{plot} and \code{summary} are available.
#' \item{biom.ident}{List of \code{V, Vstar, biomarkers}}
#' \item{V}{Values of \emph{V} statistics}
#' \item{Vstar}{Normalizes values of \emph{V} statistics (V^* values))}
#' \item{biomarkers}{Logical value, indicating if certain variable was identified as biomarker}
#' \item{diag}{Outlier diagnostics (returned only if \code{diag=TRUE})}
#' @export
#' @examples 
#' # Data simulation
#' set.seed(4523)
#' n <- 40; p <- 50
#' r <- runif(p, min = 1, max = 10)
#' conc <- runif(p, min = 0, max = 1)*5+matrix(1,p,1)*5
#' a <- conc*r
#' S <- rnorm(n,0,0.3) %*% t(rep(1,p))
#' B <- matrix(rnorm(n*p,0,0.8),n,p)
#' R <- rep(1,n) %*% t(r)
#' M <- matrix(rnorm(n*p,0,0.021),n,p)
#' # Fifth observation is an outlier
#' M[5,] <- M[5,]*3 + sample(c(0.5,-0.5),replace=TRUE,p)
#' C <- rep(1,n) %*% t(conc)
#' C[1:20,c(2,15,28,40)] <- C[1:20,c(2,15,28,40)]+matrix(1,20,4)*1.8
#' X <- (1-S)*(C*R+B)*exp(M)
#' # Biomarker identification
#' b <- biomarker(X, g1 = 1:20, g2 = 21:40, type = "tau")
biomarker <- function(x,cut=qnorm(0.975,0,1),g1,g2, type = "tau", diag=TRUE, plot=FALSE, diag.plot=FALSE){

  if (!is.data.frame(x) & !is.matrix(x)) {
    stop("Wrong x data format. Use data.frame or matrix")
  }
  if (min(x)<=0) {
    stop("Only positive values of x can be used")
  }
  
  n <- nrow(x)
  p <- ncol(x)
  n1=length(g1)
  n2=length(g2)
  
  if (max(c(g1,g2))>n) {
    stop("g1 or g2 bigger than dimensionality of x")
  }
  if (n1+n2<n) {
    warning("Not all samples from x are being used")
  }
  if (n1+n2>n) {
    warning("Some samples are selected in both groups")
  }
  if (!type=="tau" & diag==TRUE) {
    stop("Diagnostic can be performed only if type='tau'")
  }
  
  t  <- .varmatrixDiag(x,type=type)
  t1 <- .varmatrixDiag(x[g1,],type=type)
  t2 <- .varmatrixDiag(x[g2,],type=type)
  
  V <- (apply((n1*sqrt(t1$v)+n2*sqrt(t2$v))/(((n1+n2)/2)*sqrt(t$v)),2,sum,na.rm=T))  
  Vstar <- (-((V-mean(V))/(sd(V))))  
  w <- (Vstar > cut)

  
  if (diag==TRUE | diag.plot==TRUE) {
    cell <- matrix(NA,n,p)
    
    k <- 1
    for (i in c(g1,g2)){
      if (i %in% g1){
        cell[i,] <- apply(t1$d[,,i]==0,1,sum)
      }
      if (i %in% g2){
        cell[i,] <- apply(t2$d[,,k]==0,1,sum)
        k <- k+1
      }
    }
  }

  if (diag == TRUE){
    output <- list(biom.ident = list(V = V, Vstar = Vstar, biomarkers = w, names = colnames(x)),diag = cell,cut=cut) 
  }
  else {
    output <- list(biom.ident = list(V = V, Vstar = Vstar, biomarkers = w, names = colnames(x)),cut=cut)
  }

    output$name  <- "Identification of biomarkers"
    class(output)  <- "biomarker"
   
  if (diag.plot == TRUE){
    plot.biomarker(output, type="diag")
  }
  
  if (plot == TRUE){
    plot.biomarker(output, type="Vstar")
  }
  
    return(output)  
}

# Adjusted function form library(robustbase) function scaleTau2
.scaleTauDiag <- function (x, c1 = 4.5, c2 = 3, consistency = TRUE, mu.too = FALSE,
                          ...)
{
  n <- length(x)
  medx <- median(x)
  x. <- abs(x - medx)
  sigma0 <- median(x.)
  mu <- if (c1 > 0) {
    x. <- x./(sigma0 * c1)
    w <- 1 - x. * x.
    w <- ((abs(w) + w)/2)^2
    sum(x * w)/sum(w)
  }
  else medx
  x <- (x - mu)/sigma0
  rho <- x^2
  rho[rho > c2^2] <- c2^2
  if (!identical(consistency, FALSE)) {
    Erho <- function(b) 2 * ((1 - b^2) * pnorm(b) - b * dnorm(b) +
                               b^2) - 1
    Es2 <- function(c2) Erho(c2 * qnorm(3/4))
    nEs2 <- (if (consistency == "finiteSample")
      n - 2
      else n) * Es2(c2)
  }
  else nEs2 <- n
  
  return(list(tau=c(if (mu.too) mu, sigma0 * sqrt(sum(rho)/nEs2)),w=w))
}



.varmatrixDiag <- function (x, type = "sd")
{
  if (type!="sd" & type!="mad" & type!="tau" ){
    stop(paste("Type '",type,"' is not an option",sep=""))
  }
  
  rvars <- matrix(0, ncol = ncol(x), nrow = ncol(x))
  diagnost <- array(0,dim=c(dim(x)[2],dim(x)[2],dim(x)[1]))
  if (type=="mad") {
    for (i in 1:ncol(x)) {
      for (j in 1:ncol(x)) {
        if (i < j) {
          rvars[i, j] <- (mad(log(x[, i]/x[, j])))^2
          rvars[j, i] <- rvars[i, j]
        }
      }
    }
  }
  if (type=="sd") {
    for (i in 1:ncol(x)) {
      for (j in 1:ncol(x)) {
        if (i < j) {
          rvars[i, j] <- (var(log(x[, i]/x[, j])))
          rvars[j, i] <- rvars[i, j]
        }
      }
    }
  }
  if (type=="tau") {
    #library(robustbase)
    for (i in 1:ncol(x)) {
      for (j in 1:ncol(x)) {
        if (i < j) {
          ss <- .scaleTauDiag(log(x[, i]/x[, j]))
          rvars[i, j] <- (ss$tau)^2
          rvars[j, i] <- rvars[i, j]
          diagnost[i,j,] <- ss$w
          diagnost[j,i,] <- diagnost[i,j,]
          
        }
      }
    }
  }
  return(list(v=rvars,d=diagnost))
}

#' @rdname biomarker
#' @export
#' @method plot biomarker
plot.biomarker  <-  function(x,cut=qnorm(0.975,0,1),type="Vstar",...){
  if (type == "Vstar"){
    Vstar <- x$biom.ident$Vstar
    par(mar=c(4.3, 2.5 ,3.5, 1.1))    
    plot(Vstar,xlab="",ylab="",type="n",...)
    points(which(Vstar<=cut),Vstar[Vstar<=cut],xlab="",ylab="",pch=20,cex=1.2)
    points(which(Vstar>cut),Vstar[Vstar>cut],xlab="",ylab="",pch=17,cex=1.2)
    mtext(side = 3, text = expression(paste("Values of V"^"*","statistic")), cex=2, line=0.4)
    mtext(side = 1, text = "Index of Variable", cex=1.8, line=2.6)
    abline(h=cut,lty=3)
  }
  
  if (type == "diag"){
    par(las=1)
    par(mar=c(2.3, 2.5 ,3, 1.1))
    
    image(t((ncol(x$diag)-x$diag[nrow(x$diag):1,])),zlim=c(0,ncol(x$diag)),axes=F,col = grey(seq(0, 1, length = 64)))
    mtext(side=3,text="Outlier diagnostics",cex=2,line=0.7)
    mtext(side = 1,text="Variables",cex=1.5,line=0.6)
    par(las=0)
    mtext(side = 2, text = "Observations", line = 0.4,cex=1.5)
    
    p <- length(x$biom.ident$V)
    b <- sum(x$biom.ident$biomarkers)
    h <- which(x$biom.ident$biomarkers==TRUE)
    mtext(side = 3, at = h/(p-1),text=rep("*",b),cex=1,padj=0.6)
    box()
  }
}

#' @rdname biomarker
#' @export
#' @method print biomarker
#' @param ... further arguments can be passed through
print.biomarker <- function (x, ...){
  cat("Number of identified biomarkers: ",sum(x$biom.ident$biomarkers),"\n")
  
  
  if (!is.null(x$biom.ident$names)){
    cat("Identified biomarkers: ",x$biom.ident$names[x$biom.ident$biomarkers==TRUE],"\n")
  }
  else {
    cat("Positions of Identified biomarkers: ",which(x$biom.ident$biomarkers==TRUE),"\n")
  }
}


#' @rdname biomarker
#' @export
#' @method summary biomarker
#' @param object object of class biomarker
summary.biomarker  <-  function(object, ...){
  
  cat("Number of identified biomarkers: ",
      sum(object$biom.ident$biomarkers),"\n")
  
  
  if (is.null(object$biom.ident$names)){
    object$biom.ident$names  <- 1:length(object$biom.ident$V)
  }
  names(object$biom.ident$Vstar) <- object$biom.ident$names
  cat("Sorted values of V* statistic: ","\n","\n")    
  cat("Biomarkers:","\n")
  print(sort(object$biom.ident$Vstar[object$biom.ident$Vstar>object$cut],
             decreasing=TRUE))
  cat("\n")
  cat("Non-Biomarkers:","\n")
  print(sort(object$biom.ident$Vstar[object$biom.ident$Vstar<=object$cut],decreasing=TRUE))
  cat("\n")
  cat("Used cut-off value: ",object$cut)
}



