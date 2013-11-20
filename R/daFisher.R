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


