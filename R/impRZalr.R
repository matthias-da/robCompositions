impRZalr <- function(x, pos=ncol(x), dl=rep(0.05, ncol(x)-1), eps=0.0001, maxit=50, bruteforce=FALSE, method="lm", step=FALSE) {
#  testx <<- x
#  testpos <<- pos
#  testdl <<- dl
  ## some checks:	
  stopifnot(all(x[,pos] != 0 & length(which(is.na(x[,pos]))) == 0))
  if(class(x) == "matrix") x <- data.frame(x)
  if(!any(is.na(x)) && !any(x==0) ) stop("No zeros or missing values in the data")
  
  m <- matrix(rep(dl,each=nrow(x)),ncol=length(dl))
  phi <- log(m/x[,pos]) 
  xOrig <- x
  if(length(colnames) < 1) names <- letters[1:ncol(x)] else names <- colnames(x)
  nam <- colnames(x)[pos]
  x <- cbind(x[, -pos], x[, pos])
  w <- which(colnames(x) == "x[, pos]")
  colnames(x)[w] <- nam
  ## close the X rows to 1:
#  xc <- constSum(x)
  ## convert zeros into missing values:
  x[x==0] <- NA
  ## transform x into y=alr(x,pos):
  # x is already in the correct order
#  y <- alr(x, ivar=pos)$x.alr
  savey <- addLR(x, ivar=ncol(x))
  y <- savey$x.alr	
  y <- data.frame(y)
  w <- is.na(y)
  wr <- apply(y, 1, function(x) any(is.na(x)) )
  wrr <- which(wr)
  it <- 0
  d <- 99999999
  y <- as.matrix(y)
  
  ## initialisation:
  for(i in 1:length(dl)){
	 ind <- is.na(y[,i])
 #    y[ind,i] <- runif(1, 0.001 , dl[i]) # dl[i]/3*2 
	 y[ind,i] <- dl[i]/3*2 
  }

  if(any(is.na(y))) stop("NA in alrEM BEFORE")
  if(any(y=="Inf")) stop("Inf in alrEM BEFORE")	
  
#  plot(y, xlim=c(-8,4))
  
  ## start the EM:
  it <- 0
  amountMiss <- length(which(w))
  while( d > eps & it <= maxit ){
	it <- it + 1
	yold <- y
	

    for(i in 1:ncol(y)){ 
		if(method=="lm" && !step){
#			print("in standard alrEM method")
	    	lm1 <- lm( y[,i,drop=FALSE] ~ y[,-i,drop=FALSE] ) 
			yhat <- predict(lm1, new.data=y[,-i])	  
#			s <- sd(lm1$res, na.rm=TRUE)
			s <- sqrt(sum(lm1$res^2)/(nrow(y)-ncol(y)+1))	
	    }
	    if(method=="lm" && step){
			lm1 <- lm(y[,i,drop=FALSE] ~ y[,-i,drop=FALSE] ) 
		    lm1 <- stepAIC(lm1,trace=FALSE)
			yhat <- predict(lm1, new.data=y[,-i])	  
#			s <- sd(lm1$res, na.rm=TRUE)
			s <- sum(lm1$res^2)/(nrow(y)-ncol(y)+1)			
		}
       if(method=="MM" && !step) {
#		    print("in robust method")
#           if(any(is.na(y))) stop("NA in alrEM")
#			if(any(y=="Inf")) stop("Inf in alrEM")	
			lm1 <- lmrob( y[,i,drop=FALSE] ~ y[,-i,drop=FALSE])#, method="M" ) 
#			lm1 <- lmrob( y[,i,drop=FALSE] ~ y[,-i,drop=FALSE]) 
			yhat <- predict(lm1, new.data=y[,-i])	
			if(any(is.na(yhat))) stop("NA in yhat")
			if(any(yhat=="Inf")) stop("Inf in yhat")	
			s <- lm1$s		
		}
	   if(method=="MM" && step) {
		   stop("robust + stepwise is not implemented until now.")
	   }	
	    ex <- (phi[,i] - yhat)/s 
		tobit <- s*dnorm(ex)/pnorm(ex)
		tobit <- ifelse(tobit=="NaN", 0, tobit)
		tobit <- ifelse(is.na(tobit), 0, tobit)	
		tobit <- ifelse(tobit =="Inf", 0, tobit)	
		tobit <- ifelse(tobit =="-Inf", 0, tobit)	
		yhat2 <- yhat - tobit
	   # check if we are under the DL:
       if(any(yhat2[w[,i]] >= phi[w[,i],i])) stop("values above the DL are imputed.")
	   if(bruteforce){
          y[w[,i],i] <- ifelse(yhat2[w[,i]] >= phi[w[,i],i], phi[w[,i],i], yhat2[w[,i]]) 
	   } else {y[w[,i],i] <- yhat2[w[,i]] }
#	   if(i == 1) points(y[w[,i],], col=grey(it/10001), pch=2)
#	   Sys.sleep(0.2)
	}
	d <- sum(abs(y - yold))/amountMiss
}
  

  ## backtransform:	
  ximp <- suppressWarnings(addLRinv(y)) 
  
  ## change order of the column to original order:
  w <- which(colnames(ximp) == "rat")
  colnames(ximp)[w] <- nam
  colnames(ximp)[which(colnames(ximp) =="")] <- names[pos]
  ximp <- ximp[, names]
  
  ## result:
  res <- list(xOrig=xOrig, xImp=ximp, wind=NULL, iter=it, eps=eps) 
  invisible(res)
}


