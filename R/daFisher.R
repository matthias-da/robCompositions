#' Discriminant analysis by Fisher Rule.
#' 
#' Discriminant analysis by Fishers rule.
#' 
#' The Fisher rule leads only to linear boundaries. However, this method allows
#' for dimension reduction and thus for a better visualization of the
#' separation boundaries. For the Fisher discriminant rule (Fisher, 1938; Rao,
#' 1948) the assumption of normal distribution of the groups is not explicitly
#' required, although the method looses its optimality in case of deviations
#' from normality.
#' 
#' The classical Fisher discriminant rule is invariant to ilr and clr
#' transformations. The robust rule is invariant to ilr transformations if
#' affine equivariant robust estimators of location and covariance are taken.
#' 
#' Robustification is done (method \dQuote{robust}) by estimating the
#' columnwise means and the covariance by the Minimum Covariance Estimator.
#' 
#' @param x a matrix or data frame containing the explanatory variables
#' (training set)
#' @param grp grouping variable: a factor specifying the class for each
#' observation.
#' @param coda TRUE, when the underlying data are compositions.
#' @param method \dQuote{classical} or \dQuote{robust}
#' @param plotScore TRUE, if the scores should be plotted automatically.
#' @return an object of class \dQuote{daFisher} including the following
#' elements \item{B }{Between variance of the groups} \item{W }{Within variance
#' of the groups} \item{loadings}{loadings} \item{coda}{coda}
#' @author The code is was written by Peter Filzmoser. Minor modifications by
#' Matthias Templ.
#' @seealso \code{\link[rrcov]{Linda}}
#' @references Filzmoser, P. and Hron, K. and Templ, M. (2009) Discriminant
#' analysis for compositional data and robust parameter estimation.
#' \emph{Research Report SM-2009-3}, Vienna University of Technology, 27 pages.
#' 
#' Fisher, R. A. (1938) The statistical utiliziation of multiple measurements.
#' \emph{Annals of Eugenics}, 8:376-386.
#' 
#' Rao, C.R. (1948) The utilization of multiple measurements in problems of
#' biological classification. \emph{Journal of the Royal Statistical Society},
#' Series B, 10:159-203.
#' @keywords multivariate
#' @examples
#' 
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
#' 
daFisher <- function(x,grp,coda=TRUE,method="classical",plotScore=FALSE)
{

if(class(x)=="data.frame") x <- as.matrix(x)
# Fisher LDA:
if(length(grp) != dim(x)[1]){
	stop(paste("grp must be of length",dim(x)[1]))
}
if(dim(x)[2] < 1){
	stop("matrix or data.frame expected.")
}
if(coda){
	x <- isomLR(x)
}

	p <- ncol(x)
	ni <- table(grp)
	ng <- length(ni)
	n <- sum(ni)
	pi <- ni/n
if (method=="classical"){
  muil <- by(x,factor(grp),colMeans)
  sigil <- by(x,factor(grp),cov)
}
else {
#  require(rrcov)
  res <- by(x,factor(grp),CovMcd)
  muil <- lapply(res,getCenter)
  sigil <- lapply(res,getCov)
}

	mui <- matrix(unlist(muil),ng,p,byrow=TRUE)
	mu <- pi%*%mui
	hlp <- diag(sqrt(pi))%*%(mui-rep(1,ng)%*%mu)
	B <- t(hlp)%*%hlp
	sigi <- array(unlist(sigil),dim=c(p,p,ng))
	W <- apply(sigi*array(sort(rep(pi,p*p)),dim=c(p,p,ng)),c(1,2),sum)
	adir <- matrix(as.numeric(eigen(solve(W)%*%B)$vec),ncol=p)
	adirs <- t(t(adir)/(sqrt(diag(t(adir)%*%W%*%adir))))
	scores=x%*%adirs
if(plotScore){
  pl <- as.numeric(factor(grp))
  plot(scores[,1:2],col=pl, pch=pl, cex=1.5, xlab="Scores 1", ylab="Scores 2", cex.lab=1.2)
  legend("topright", legend=levels(factor(grp)), pch=unique(pl), col=unique(pl), cex=1.3)
}
#    postgroup <- apply(scores, 1, which.min)
#	print(postgroup)
	res <- list(B=B,W=W,loadings=adir,scores=scores,#classification=postgroup, 
			mu=muil, sigma=sigil,
			coda=coda)
	class(res) <- "daFisher"
	
#	## fill in for class lda
#	g <- as.factor(grp)
#	lev <- lev1 <- levels(g)
#	counts <- as.vector(table(g))
#	prior <- counts/n
#	prior <- prior[counts > 0]
#
#if(method == "moment") fac <- 1/(n-ng) else fac <- 1/n
#X <- sqrt(fac) * (x - group.means[g,  ]) %*% scaling
#X.s <- svd(X, nu = 0)
#X <-  sqrt(nu/(nu-2)*(1 + p/nu)/n * w) * (x - group.means[g,  ]) %*% scaling
#X.s <- svd(X, nu = 0)
#	cl <- match.call()
#	cl[[1L]] <- as.name("daFisher")
#	
#	res <- structure(list(prior = prior, counts = counts, means = mui,
#					scaling = hlp, lev = lev, svd = hlp,
#					N = n, call = cl, B=B, W=W, loadings=adir, coda=coda),
#			class = "lda")
#
#	z1=z[grp=="arabica",]
#	z2=z[grp=="blended",]
#	n1=nrow(z1)
#	n2=nrow(z2)
#	n=n1+n2
#	p1=n1/n
#	p2=n2/n
#	m1=apply(z1,2,mean)
#	m2=apply(z2,2,mean)
#	S1=cov(z1)
#	S2=cov(z2)
#	Sp=((n1-1)/(n1-1+n2-1))*S1+((n2-1)/(n1-1+n2-1))*S2
#	Sp1=solve(Sp)
#	yLDA=as.numeric(t(m1-m2)%*%Sp1%*%t(z)-as.numeric(1/2*t(m1-m2)%*%Sp1%*%(m1+m2)))-log(p2/p1)	
#	plot(z, pch=21, bg=ifelse(grp=="arabica","red","blue"))#bg=ifelse(yLDA<0,"red","blue"))
#	y1=seq(from=min(z[,1])-1.5,to=max(z[,1])+1.9,by=0.05)
#	y2=seq(from=min(z[,2]),to=max(z[,2])+0.2,by=0.05)
#	y1a=rep(y1,length(y2))
#	y2a=sort(rep(y2,length(y1)))
#	ya=cbind(y1a,y2a)
#	yaLDA=as.numeric(t(m1-m2)%*%Sp1%*%t(ya)-
#					as.numeric(1/2*t(m1-m2)%*%Sp1%*%(m1+m2)))-log(p2/p1)
#	
#	boundLDA=abs(yaLDA)<0.05
#	lines(lowess(y1a[boundLDA],y2a[boundLDA]),col=gray(0.6),lwd=1.5,lty=1)

invisible(res)
}


