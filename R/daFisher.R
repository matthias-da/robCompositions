#' @importFrom e1071 matchClasses
NULL

#' Discriminant analysis by Fisher Rule.
#' 
#' Discriminant analysis by Fishers rule using the logratio approach to compositional data.
#' 
#' The Fisher rule leads only to linear boundaries. However, this method allows
#' for dimension reduction and thus for a better visualization of the
#' separation boundaries. For the Fisher discriminant rule (Fisher, 1938; Rao,
#' 1948) the assumption of normal distribution of the groups is not explicitly
#' required, although the method looses its optimality in case of deviations
#' from normality.
#' 
#' The classical Fisher discriminant rule is invariant to ilr coordinates and clr
#' coefficients. The robust rule is invariant to ilr transformations if
#' affine equivariant robust estimators of location and covariance are taken.
#' 
#' Robustification is done (method \dQuote{robust}) by estimating the
#' columnwise means and the covariance by the Minimum Covariance Estimator.
#' 
#' @aliases daFisher print.daFisher
#' @param x a matrix or data frame containing the explanatory variables
#' (training set)
#' @param grp grouping variable: a factor specifying the class for each
#' observation.
#' @param coda TRUE, when the underlying data are compositions.
#' @param method \dQuote{classical} or \dQuote{robust} estimation.
#' @param plotScore TRUE, if the scores should be plotted automatically.
#' @param ... additional arguments for the print method passed through
#' @importFrom e1071 matchClasses
#' @return an object of class \dQuote{daFisher} including the following
#' elements 
#' \item{B }{Between variance of the groups} 
#' \item{W }{Within variance
#' of the groups} 
#' \item{loadings}{loadings} 
#' \item{scores}{fisher scores} 
#' \item{mc}{table indicating misclassifications} 
#' \item{mcrate}{misclassification rate}  
#' \item{coda}{coda}
#' \item{grp}{grouping}
#' \item{grppred}{predicted groups}
#' \item{xc}{xc}
#' \item{meanj}{meanj}
#' \item{cv}{cv}
#' \item{pj}{pj}
#' \item{meanov}{meanov}
#' \item{fdiscr}{fdiscr}
#' @author Peter Filzmoser, Matthias Templ.
#' @seealso \code{\link[rrcov]{Linda}}
#' @references Filzmoser, P. and Hron, K. and Templ, M. (2012) 
#' Discriminant analysis for compositional data and robust parameter estimation. 
#' \emph{Computational Statistics}, 27(4), 585-604.
#' 
#' Fisher, R. A. (1938) The statistical utiliziation of multiple measurements.
#' \emph{Annals of Eugenics}, 8, 376-386.
#' 
#' Rao, C.R. (1948) The utilization of multiple measurements in problems of
#' biological classification. \emph{Journal of the Royal Statistical Society},
#' Series B, 10, 159-203.
#' @keywords multivariate
#' @export
#' @import rrcov MASS
#' @examples
#' ## toy data (non-compositional)
#' require(MASS)
#' x1 <- mvrnorm(20,c(0,0,0),diag(3))
#' x2 <- mvrnorm(30,c(3,0,0),diag(3))
#' x3 <- mvrnorm(40,c(0,3,0),diag(3))
#' X <- rbind(x1,x2,x3)
#' grp=c(rep(1,20),rep(2,30),rep(3,40))
#' 
#' #par(mfrow=c(1,2))
#' d1 <- daFisher(X,grp=grp,method="classical",coda=FALSE)
#' d2 <- daFisher(X,grp=grp,method="robust",coda=FALSE)
#' d2
#' summary(d2)
#' predict(d2, newdata = X)
#' 
#' ## example with olive data:
#'\dontrun{
#' data(olive, package = "RnavGraph")
#' # exclude zeros (alternatively impute them if 
#' # the detection limit is known using impRZilr())
#' ind <- which(olive == 0, arr.ind = TRUE)[,1]
#' olives <- olive[-ind, ]
#' x <- olives[, 4:10]
#' grp <- olives$Region # 3 groups
#' res <- daFisher(x,grp)
#' res
#' summary(res)
#' res <- daFisher(x, grp, plotScore = TRUE)
#' res <- daFisher(x, grp, method = "robust")
#' res
#' summary(res)
#' predict(res, newdata = x)
#' res <- daFisher(x,grp, plotScore = TRUE, method = "robust")
#' 
#' # 9 regions
#' grp <- olives$Area
#' res <- daFisher(x, grp, plotScore = TRUE)
#' res
#' summary(res)
#' predict(res, newdata = x)
#' }


daFisher <- function(x, grp, coda=TRUE,
                     method = "classical",
                     # rule="fisher",
                     plotScore = FALSE, ...){
  ## some checks
  if(class(x) == "data.frame") x <- as.matrix(x)
  ## Fisher LDA:
  if(length(grp) != dim(x)[1]){
    stop(paste("grp must be of length", dim(x)[1]))
  }
  if(dim(x)[2] < 1){
    stop("matrix or data.frame expected.")
  }
  if(coda){
    x <- pivotCoord(x)
  }
  n <- nrow(x)
  p <- ncol(x)
  grp <- as.factor(grp)
  glev <- levels(grp)
  g <- length(glev)
  pj <- rep(NA,g)
  meanj <- matrix(NA,nrow=p,ncol=g)
  cv <- list()
  for (j in 1:g){
    pj[j] <- sum(grp==glev[j])/n
    if(method == "classical"){
      meanj[,j] <- apply(x[grp==glev[j],],2,mean)
      cv[[j]] <- cov(x[grp==glev[j],])
    } else {
      robcov <- covMcd(x[grp==glev[j],])
      meanj[,j] <- robcov$center
      cv[[j]] <- robcov$cov
      #   else {
      #   #  require(rrcov)
      #     res <- by(x,factor(grp),CovMcd)
      #     muil <- lapply(res,getCenter)
      #     sigil <- lapply(res,getCov)
      #   }
    }
  }
  
  #  if(rule=="lda"){
  #    if(method=="robust"){
  #      res <- Linda(x, grp)
  #    }
  #    else
  #      res <- lda(x, grp)
  #  }
  #  else if(rule=="qda"){
  #    res <- qda(x, grp)
  #  }
  #  else{
  ###meanov <- t(t(meanj)*pj)
  meanov <- meanj%*%pj
  B <- matrix(0,p,p)
  W <- matrix(0,p,p)
  for (j in 1:g){
    ###B <- B+pj[j]*((meanj-meanov)%*%t(meanj-meanov))
    B <- B+pj[j]*((meanj-meanov%*%rep(1,g))%*%t(meanj-meanov%*%rep(1,g)))
    #    W <- W+pj[j]*cov(x[grp==glev[j],])
    W <- W+pj[j]*cv[[j]]
  }
  l <- min(g-1,p) # use this number of components
  #V=matrix(Re(eigen(solve(W)%*%B)$vec)[,1:l],ncol=l)
  #V=t(t(V)/(sqrt(diag(t(V)%*%W%*%V))))
  
  # besser:
  B.svd <- svd(B)
  l1 <- length(B.svd$d>1e-6)
  B12 <- B.svd$u[, 1:l1] %*% diag(sqrt(B.svd$d[1:l1])) %*% t(B.svd$u[, 1:l1])
  Bm12 <- B.svd$u[, 1:l1] %*% diag(1/sqrt(B.svd$d[1:l1])) %*% t(B.svd$u[, 1:l1])
  K <- eigen(B12 %*% solve(W) %*% t(B12))
  l2 <- min(g - 1, p)
  l <- min(length(K$val>1e-6),l2)
  
  Vs <- Bm12%*%K$vec[,1:l]
  V <- t(t(Vs)/(sqrt(diag(t(Vs)%*%W%*%Vs))))
  
  
  # Fisher scores
  fs <- matrix(NA,nrow=n,ncol=g)
  dimnames(fs)[[2]] <- glev
  for (j in 1:g){
    xc <- scale(x,meanj[,j],scale=FALSE)
    xproj <- xc%*%V
    fs[,j] <- sqrt(apply(xproj^2,1,sum)-2*log(pj[j]))
  }
  
  ## prediction:
  grppred <- glev[apply(fs, 1, which.min)]
  
  ## misclassification rate:
  mc <- table(grp, grppred)
  mc <- mc[, matchClasses(mc, method = "exact")]
  rate <- 1 - sum(diag(mc)) / sum(mc)
  
  fdiscr <- scale(x,meanov,FALSE)%*%V[,1:2] # discriminant scores
  
  ## plot scores (if TRUE)
  if(plotScore){
    #proj <- xc %*%V [,1:2]
    ###proj <- fs[,1:2]
    proj <- data.frame(fdiscr)
    proj$grp <- as.factor(grp)
    proj$grppred <- as.factor(grppred)
    firstscores <- NULL
    secondscores <- NULL
    colnames(proj) <- c("firstscores", "secondscores","grp", "grppred")
    gg <- ggplot(proj, aes(firstscores, secondscores, colour = grp, shape = grppred)) 
    gg <- gg + geom_point()
    gg <- gg + xlab("First Fisher scores") + ylab("Second Fisher scores")
    print(gg)
    #    plot(, col=grp, pch=grppred, 
    #         xlab="first fisher scores", ylab="second fisher scores")
    #    }
  }
  
    res <- list(B = B, W = W, loadings = V, scores = fs, mc = mc, mcrate = rate, 
                coda=coda, grp=grp, grppred=grppred, xc=xc, meanj=meanj, cv=cv, 
                pj=pj, meanov=meanov, fdiscr = fdiscr)
    class(res) <- "daFisher"
  
  res
}

# daFisher <- function(x, grp, coda=TRUE,
#                    method = "classical",
#                    plotScore = FALSE){
#   ## some checks
#   if(class(x) == "data.frame") x <- as.matrix(x)
#   ## Fisher LDA:
#   if(length(grp) != dim(x)[1]){
#     stop(paste("grp must be of length", dim(x)[1]))
#   }
#   if(dim(x)[2] < 1){
#     stop("matrix or data.frame expected.")
#   }
#   if(coda){
#     x <- pivotCoord(x)
#   }
#   n <- nrow(x)
#   p <- ncol(x)
#   glev <- unique(grp)
#   g <- length(glev)
#   pj <- rep(NA,g)
#   meanj <- matrix(NA,nrow=p,ncol=g)
#   cv <- list()
#   for (j in 1:g){
#     pj[j] <- sum(grp==glev[j])/n
#     if(method == "classical"){
#       meanj[,j] <- apply(x[grp==glev[j],],2,mean)
#       cv[[j]] <- cov(x[grp==glev[j],])
#     } else {
#       robcov <- covMcd(x[grp==glev[j],])
#       meanj[,j] <- robcov$center
#       cv[[j]] <- robcov$cov
#       #   else {
#       #   #  require(rrcov)
#       #     res <- by(x,factor(grp),CovMcd)
#       #     muil <- lapply(res,getCenter)
#       #     sigil <- lapply(res,getCov)
#       #   }
#     }
#   }
#   
#   meanov <- t(t(meanj)*pj)
#   B <- matrix(0,p,p)
#   W <- matrix(0,p,p)
#   for (j in 1:g){
#     B <- B+pj[j]*((meanj-meanov)%*%t(meanj-meanov))
# #   W <- W+pj[j]*cov(x[grp==glev[j],])
#     W <- W+pj[j]*cv[[j]]
#   }
#   l <- min(g-1,p) # use this number of components
#   #V=matrix(Re(eigen(solve(W)%*%B)$vec)[,1:l],ncol=l)
#   #V=t(t(V)/(sqrt(diag(t(V)%*%W%*%V))))
#     
#   # besser:
#   B.svd <- svd(B)
#   B12 <- B.svd$u[,1:l]%*%diag(sqrt(B.svd$d[1:l]))%*%t(B.svd$u[,1:l])
#   Bm12 <- B.svd$u[,1:l]%*%diag(1/sqrt(B.svd$d[1:l]))%*%t(B.svd$u[,1:l])
#   K <- eigen(B12%*%solve(W)%*%B12)
#   Vs <- Bm12%*%K$vec[,1:l]
#   V <- t(t(Vs)/(sqrt(diag(t(Vs)%*%W%*%Vs))))
#     
#     
#   # Fisher scores
#   fs=matrix(NA,nrow=n,ncol=g)
#   for (j in 1:g){
#     xc <- scale(x,meanj[,j],scale=FALSE)
#     xproj <- xc%*%V
#     fs[,j] <- sqrt(apply(xproj^2,1,sum)-2*log(pj[j]))
#   }
#     
#   ## predition:
#   grppred <- apply(fs, 1, which.min)
#     
#   ## misclassification rate:
#   mc <- table(grp, grppred)
#   mc <- mc[, matchClasses(mc, method = "exact")]
#   rate <- 1 - sum(diag(mc)) / sum(mc)
#     
#   ## plot scores (if TRUE)
#   if(plotScore){
#     #proj <- xc %*%V [,1:2]
#     proj <- fs[,1:2]
#     proj <- data.frame(proj)
#     proj$grp <- as.factor(grp)
#     proj$grppred <- as.factor(grppred)
#     firstscores <- NULL
#     secondscores <- NULL
#     colnames(proj) <- c("firstscores", "secondscores","grp", "grppred")
#     gg <- ggplot(proj, aes(firstscores, secondscores, colour = grp, shape = grppred)) 
#     gg <- gg + geom_point()
#     gg <- gg + xlab("first fisher scores") + ylab("second fisher scores")
#     print(gg)
#     #    plot(, col=grp, pch=grppred, 
#     #         xlab="first fisher scores", ylab="second fisher scores")
#   }
#     
#   res <- list(B = B, 
#               W = W,
#               loadings = V,
#               scores = fs,#classification=postgroup, 
#               #  mu=muil, 
#               #  sigma=sigil,
#               mc = mc,
#               mcrate =  rate,
#               coda=coda,
#               grp=grp, grppred=grppred, xc=xc)
#   class(res) <- "daFisher"
# 
#   
#   res
# }



#' @rdname daFisher
#' @method print daFisher
#' @export
print.daFisher <- function(x,...){
  cat("--------------------------------------")
  cat("\nResults from Fishers discriminant analysis, coda ==", x$coda)
  cat("\n- Variance between the classes: \n")
  print(x$B)
  cat("\n- Variance within the classes: \n")
  print(x$W)
  cat("\n- Loadings matrix: \n")
  print(x$load)
  cat("--------------------------------------\n")
}

#' @rdname daFisher
#' @method predict daFisher
#' @param object object of class \dQuote{daFisher}
#' @param newdata new data in the appropriate form (CoDa, etc)
#' @export
predict.daFisher <- function(object, ..., newdata){
  # res ... result object of daFisher
  # newdata ... new data in the appropriate form (CoDa, etc)
  
  g <- ncol(object$meanj) # number of groups
  if (object$coda){
    newdata <- pivotCoord(newdata)
  }
  # Fisher scores
  fs <- matrix(NA,nrow=nrow(newdata),ncol=g)
  dimnames(fs)[[2]] <- dimnames(object$mc)[[1]]
  for (j in 1:g){
    xc <- scale(newdata,object$meanj[,j],scale=FALSE)
    xproj <- xc%*%object$loadings
    fs[,j] <- sqrt(apply(xproj^2,1,sum)-2*log(object$pj[j]))
  }
  
  ## prediction:
  grp <- apply(fs, 1, which.min)
  grpnam <- colnames(fs)[grp]
  
  list(grpnam=grpnam,grp=grp)    
  
}

#' @rdname daFisher
#' @method summary daFisher
#' @export
summary.daFisher <- function(object, ...){
  cat("--------------------------------------")
  cat("\nMisclassification rate from Fishers discriminant analysis, coda ==", object$coda)
  cat("\n")
  print(object$mcrate)
  cat("\n--------------------------------------")
  cat("\nMisclassifications from Fishers discriminant analysis, coda ==", object$coda)
  cat("\n")
  print(object$mc)
  cat("\n--------------------------------------\n")
}
