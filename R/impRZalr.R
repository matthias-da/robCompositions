impRZalr <- function(x, pos=ncol(x), dl=rep(0.05, ncol(x)-1), 
                     eps=0.0001, maxit=50, bruteforce=FALSE, 
                     method="lm", step=FALSE, nComp = "boot", R=10,
                     verbose=FALSE) {

#   x <- xorig <- genVarsSimple(n=50,p=30)
#   qu <- apply(x, 2, quantile, 0.05)
#   for(i in 1:(ncol(x)-2)){
#      x[x[,i] < qu[i], i] <- 0
#   }
#   pos <- ncol(x)-1
#   x <- data.frame(x)
#   dl <- qu
#   method="lm"
#   step=FALSE
#   bruteforce=FALSE
  
  ## some checks:	
  if(is.matrix(x)) stop("convert to data.frame first")
  stopifnot(all(x[,pos] != 0 & length(which(is.na(x[,pos]))) == 0))
  if(!any(is.na(x)) && !any(x==0) ) stop("No zeros or missing values in the data")
  if(method=="pls" & ncol(x)<5) stop("too less variables/parts for method pls")
  if(is.null(nComp)){
    pre <- FALSE
    nC <- NULL
  } else if(nComp=="boot"){
    nC <- integer(ncol(x))
    pre <- TRUE
  } else if(length(nComp) == ncol(x)){
    nC <- nComp
    pre <- FALSE
  } else  {
    pre <- FALSE	
  }
  
  ## zeros to NA:
  x[x==0] <- NA
  
  ## transformation:
  xa <- addLR(x, ivar=pos)
  xax <- xa$x
  w <- is.na(xa$x)
  
  ## dl --> phi
  m <- matrix(rep(dl, each=nrow(x)), ncol=length(dl))
  phi <- log(m/x[,pos]) 
  phi <- phi[,-pos]
  xOrig <- x

  it <- 0
  d <- 99999999
  
  ## initialisation:
  xax[w] <- phi[w] * 2 / 3
  
  ## start the EM:
  it <- 0
  amountMiss <- length(which(w))
  while( d > eps & it <= maxit ){
	it <- it + 1
	yold <- xax
  n2 <- nrow(xax)-ncol(xax)+1
  cn <- colnames(x)
  n <- nrow(x)
  for(i in 1:ncol(xax)){ 
    response <- xax[,i]
    predictors <- as.matrix(xax[,-i])
		if(method=="lm" && !step){
	    lm1 <- lm(response ~ predictors)
  			yhat <- predict(lm1, new.data=predictors)	  
  			s <- sd(lm1$res, na.rm=TRUE)
	#  		s <- sqrt(sum(lm1$res^2)) / n2	
	  }
    if(method=="pls" && !step){    
      if(it == 1 & pre){ ## evaluate ncomp.
        nC[i] <- bootnComp(predictors,
                           y=response, R, 
                           plotting=FALSE)$res #$res2
      }
      if(verbose) cat("\n ncomp for part",i,":",nC[i])
      reg1 <- mvr(as.matrix(response) ~ as.matrix(predictors), ncomp=nC[i],
                  method="simpls")
      yhat <- predict(reg1, new.data=data.frame(predictors), ncomp=nC[i])
      s <- sqrt(sum(reg1$res^2)/n) 
      
      lm1 <- lm(response ~ predictors)
      yhat <- predict(lm1, new.data=predictors)	  
    		s <- sd(lm1$res, na.rm=TRUE)
      # s <- sqrt(sum(lm1$res^2)) / n2	
    }
    if(method=="lm" && step){
	  	lm1 <- lm(response ~ predictors) 
	    lm1 <- stepAIC(lm1, trace=FALSE)
	  	yhat <- predict(lm1, new.data=predictors)	  
    	s <- sd(lm1$res, na.rm=TRUE)
  	#	s <- sqrt(sum(lm1$res^2)) / n2			
	 	}
    if(method=="MM" && !step) {
  		lm1 <- lmrob(response ~ predictors)
  		yhat <- predict(lm1, new.data=predictors)	
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
    if(any(yhat2[w[,i]] >= phi[w[,i],i])) stop(paste("values above the DL are imputed. \n column",i))
	   if(bruteforce){
          xax[w[,i],i] <- ifelse(yhat2[w[,i]] >= phi[w[,i],i], phi[w[,i],i], yhat2[w[,i]]) 
	   } else {
       xax[w[,i],i] <- yhat2[w[,i]] 
	   }
	}
	d <- sum(abs(xax - yold))/amountMiss
}

  ## backtransform:	
  xa$x.alr <- xax
  ximp <- suppressWarnings(addLRinv(xa)) 
  
  ## result:
  res <- list(x=ximp, wind=NULL, iter=it, eps=eps) 
  invisible(res)
}


