`impCoda` <-
function(x, maxit=10, eps=0.5, method="ltsReg", closed=FALSE, 
		init="KNN", k=5, dl=rep(0.05, ncol(x)), noise=0.1, bruteforce=FALSE){

	## MT & KH, 1. Version April 2008
	## MT 01. August 2008 (modification).
	## MT 17. Oktober 2008 (adaption)
	## for method pca: classical, mcd, gridMAD
	## for regression: lm, ltsReg
	## if closed  == FALSE, ilr is applied.

#	`ilrM` <-
#			function(x){
#		x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
#		D=ncol(x)
#		for (i in 1:ncol(x.ilr)){
#			x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
#		} 
#		invisible(-x.ilr)
#	}
#	`invilrM` <-
#			function(x.ilr){
#		y=matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
#		D=ncol(x.ilr)+1
#		y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
#		for (i in 2:ncol(y)){
#			for (j in 1:(i-1)){
#				y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
#			}
#		}
#		for (i in 2:(ncol(y)-1)){
#			y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
#		}
#		yexp=exp(-y)
#		x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
#		invisible(x.back)
##return(yexp)
	#}
	
	
	
	if( is.vector(x) ) stop("x must be a matrix or data frame")
	stopifnot((method %in% c("ltsReg", "ltsReg2", "classical", "lm", "roundedZero","roundedZeroRobust")))
	if( k > nrow(x)/4 ) warning("k might be too large")
#	if(method == "roundedZero") init <- "roundedZero"

	xcheck <- x

#	if(method == "roundedZero"){
#		x[x==0] <- NA
#	}


	##index of missings / non-missings
	w <- is.na(x)
	wn <- !is.na(x)
	w2 <- apply(x, 1, function(x){
          length(which(is.na(x)))
	})



	
	if(method == "gmean"){
	### mean imputation im Simplex:
	geometricmean <- function (x) {
	    if (any(na.omit(x == 0)))
	        0
	    else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
	}
	gm <- apply(x, 2, function(x) {
	  geometricmean(as.numeric(x[complete.cases(x)]))
	})
	
	xmean <- x
	for(i in 1:ncol(x)){
	  xmean[w[,i], i] <- gm[i]
	}
	res <- list(xOrig=xcheck, xImp=xmean, criteria=0, iter=0, maxit=maxit, w=length(which(w)), wind=w)
	} else if ( method=="meanClosed" ){
	  xmean <- x
	  impute <-                          
	  function (x, what = c("median", "mean")) 
	  {                                        
		  what <- match.arg(what)              
		  if (what == "median") {              
			  retval <- apply(x, 2, function(z) {
						  z[is.na(z)] <- median(z, na.rm = TRUE)
						  z                                     
					  })                                        
		  }
		  else if (what == "mean") {
			  retval <- apply(x, 2, function(z) {
						  z[is.na(z)] <- mean(as.numeric(z), na.rm = TRUE)
						  z
					  })
		  }
		  else {
			  stop("`what' invalid")
		  }
		  retval
	  }	  
	  xmean <- impute(xmean)
	  res <- list(xOrig=xcheck, xImp=xmean, criteria=0, iter=0, maxit=maxit, w=length(which(w)), wind=w)
	} else{
	
	
		
		##sort the columns of the data according to the amount of missings in the variables
		
		indM <- sort(apply(x,2,function(x) length(which(is.na(x)))),index.return=TRUE,decreasing=TRUE)$ix
		cn <- colnames(x)
		
		## first step - replace all NAs with values with 'nearest neighbour' algorithm
		
		#if(init=="NN"){
		#  x <- templdist.C(x)
		#}
		if(init=="KNN"){
		  x <- impKNNa(x, k=k, metric="Aitchison", normknn=TRUE)$xImp #"Aitchison"
		}
		if(init=="KNNclosed"){
		  x <- impKNNa(x, k=k, metric="Euclidean")$xImp
		}
		if(init=="roundedZero"){
		  x[is.na(x)] <- 0.001
		}
		if(init=="geometricmean"){
		  gm <- apply(x, 2, function(x) geometricmean(x[!is.na(x)]))
		  for(i in 1:ncol(x)){
			  x[is.na(x[,i]),i] <- gm[[i]]
		  }
		}
		
		
		
		#x=acomp(x) #Aitchison compositions (for ilr)
		#x2 <- acomp(xcheck) # with missings
		
		##PCA algorithmus
		
		it=0
		criteria <- 10000000
		error <- rep(0, ncol(x))
		
		###########################################
		###  start the iteration
		
		##ternary(acomp(x))
		#plot(ilr(x[w2==0,]), xlim=c(-5,5), ylim=c(-8,0.5))
		#points(ilr(x[w2>0,]), col=gray(0.9), pch=3)
		#gr <- seq(0.7,0.3, length.out=8)
		
		while(it <= maxit & criteria >= eps){
	
	  		xold <- x
	  		it=it+1
	  		for(i in 1:ncol(x)){
			    #change the first column with that one with the highest amount of NAs
			    #in the step
			    xNA=x[,indM[i]]
			    x1=x[,1]
			    x[,1]=xNA
			    x[,indM[i]]=x1
			
			    if( closed == FALSE ) xilr=isomLR(x) else xilr=x
			
			    #apply the PCA algorithm -> ximp
			    ind <- cbind(w[, indM[i]], rep(FALSE, dim(w)[1]))
			    if(method=="classical" | method =="mcd" | method == "gridMAD"){
			      	xilr <- impPCA(xilr, indexMiss=ind, eps=1,
			               indexObs=!ind, method=method)
			    }
			
				#if( method == "em" ){
				#  s <- prelim.norm(as.matrix(xilr)) 
				#  thetahat <- em.norm(s, showits=FALSE)   
				#  xilr <- imp.norm(s, thetahat, as.matrix(xilr))   
				#}
				#
				#if( method == "lls" ){
				#  xilr <- suppressWarnings(llsImpute(xmiss, 3, verbose = FALSE)@completeObs)
				#}
			
				if(method == "ltsReg" | method == "lm"){
			      	#beta=ltsReg(xilr[,1]~xilr[,2],xilr)$coefficients
			  	  	xilr <- data.frame(xilr)
			  		c1 <- colnames(xilr)[1]
			  		colnames(xilr)[1] <- "V1"
			  		reg1 = get(method)(V1 ~ ., data=xilr)
			  		colnames(xilr)[1] <- c1
			  		##imp= cbind(rep(1, nrow(xilr)), xilr[,-1]) %*% reg1$coef  
			  		xilr[w[, indM[i]], 1] <- fitted(reg1)[w[, indM[i]]]   ##imp[w[, indM[i]]] ## xilr[w[, indM[i]], 1]
				}
				if(method == "ltsReg2"){
				  xilr <- data.frame(xilr)
				  c1 <- colnames(xilr)[1]
				  colnames(xilr)[1] <- "V1"
				  reg1 = ltsReg(V1 ~ ., data=xilr)
				  imp= as.matrix(cbind(rep(1, nrow(xilr)), xilr[,-1])) %*% reg1$coef 
				  colnames(xilr)[1] <- c1
			      ##imp= cbind(rep(1, nrow(xilr)), xilr[,-1]) %*% reg1$coef  
				  xilr[w[, indM[i]], 1] <- fitted(reg1)[w[, indM[i]]]  
				  error[indM[i]] <- noise*sd(xilr[,1])#sqrt(mad(xilr[,1]))
				  #+  
				  #    rnorm(length(imp[w[, indM[i]]]), 0, sd=0.5*sqrt(mad(xilr[,1]))) 
				  #  xilr <- data.frame(xilr)
				  ###imp[w[, indM[i]]] + rnorm(length(imp[w[, indM[i]]]), 0, sd=0.5*sqrt(mad(xilr[,1]))) 
				}
#				if(method == "roundedZero"){
#					xilr <- ilrM(x)
#					phi <- ilr(cbind(rep(dl[indM[i]], nrow(x)), x[,-1,drop=FALSE]))[,1]
#					xilr <- data.frame(xilr)
#					c1 <- colnames(xilr)[1]
#					colnames(xilr)[1] <- "V1"
#					reg1 = lm(V1 ~ ., data=xilr)
#					yhat2 <- predict(reg1, new.data=xilr[,-i]) 	
#					#colnames(xilr)[1] <- c1
#					#s <- sd(xilr[,1], na.rm=TRUE)
#					#ex <- (phi - yhat)/s
#					#yhat2 <- yhat - s*dnorm(ex)/pnorm(ex)
#					if(bruteforce){ 
#						xilr[w[, indM[i]], 1] <- ifelse(yhat2[w[, indM[i]]] <= phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2[w[, indM[i]]] )
#					} else {
#						s <- sd(reg1$res, na.rm=TRUE)
#						ex <- (phi - yhat2)/s 
#						yhat2 <- yhat2 - s*dnorm(ex)/pnorm(ex)
#						xilr[w[, indM[i]], 1] <- yhat2[w[, indM[i]]]
#			        }
#				}
#				if(method == "roundedZeroRobust"){
#					xilr <- ilrM(x)
#					phi <- ilr(cbind(rep(dl[indM[i]], nrow(x)), x[,-1,drop=FALSE]))[,1]
#					xilr <- data.frame(xilr)
#					c1 <- colnames(xilr)[1]
#					colnames(xilr)[1] <- "V1"
#					reg1 = rlm(V1 ~ ., data=xilr, method="MM")
#					yhat2 <- predict(reg1, new.data=xilr[,-i]) 	
#					#colnames(xilr)[1] <- c1
#					#s <- sd(xilr[,1], na.rm=TRUE)
#					#ex <- (phi - yhat)/s
#					#yhat2 <- yhat - s*dnorm(ex)/pnorm(ex)
#					if(bruteforce){ 
#						xilr[w[, indM[i]], 1] <- ifelse(yhat2[w[, indM[i]]] <= phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2[w[, indM[i]]] )
#					} else {
##						s <- mad(reg1$res, na.rm=TRUE)
	##					s <- reg1$s
		#				ex <- (phi - yhat2)/s 
		#				yhat2 <- yhat2 - s*dnorm(ex)/pnorm(ex)
		#				xilr[w[, indM[i]], 1] <- yhat2[w[, indM[i]]]
		##			}
			#	}
				#if( method == "rf" ){
				#  xilr[w[, indM[i]], 1] <- NA
				#  reg1 <- rfImpute(xilr[,1] ~ xilr[,-1], data=xilr)
				#  xilr[w[, indM[i]], 1] <- reg1[w[, indM[i]]] 
				#}
			
				if( closed == FALSE ) x=isomLRinv(xilr) else x=xilr
#				if( closed == FALSE && method %in% c("roundedZero","roundedZeroRobust")) x=invilrM(xilr) else x=xilr			
				#return the order of columns
			
				xNA=x[,1]
				x1=x[,indM[i]]
				x[,1]=x1
				x[,indM[i]]=xNA
	
	
	 	   }
	
	
	
	  	  criteria <- sum( ((xold - x)/x)^2, na.rm=TRUE) #sum(abs(as.matrix(xold) - as.matrix(x)), na.rm=TRUE)  ## DIRTY: (na.rm=TRUE)
          #print(paste(method, ",", it, ",", "criteria=",round(criteria,3)))
		  if(closed == FALSE) colnames(x) <- colnames(xcheck)
		
		}
		
		if( method == "ltsReg2"){ # finally, add an error for method ltsReg2 
			for(i in 1:ncol(x)){
				xNA=x[,indM[i]]
				x1=x[,1]
				x[,1]=xNA
				x[,indM[i]]=x1
				if( closed == FALSE ) xilr=isomLR(x) else xilr=x
				  ind <- cbind(w[, indM[i]], rep(FALSE, dim(w)[1]))	
				  xilr <- data.frame(xilr)
				  #c1 <- colnames(xilr)[1]
				  #colnames(xilr)[1] <- "V1"
				  #reg1 = ltsReg(V1 ~ ., data=xilr)
				  #imp= as.matrix(cbind(rep(1, nrow(xilr)), xilr[,-1])) %*% reg1$coef 
				  #colnames(xilr)[1] <- c1
				  xilr[w[, indM[i]], 1] <- xilr[w[, indM[i]], 1] +  
				    rnorm(length(which(w[, indM[i]])), 0, sd=error[indM[i]]) 
				  xilr <- data.frame(xilr)
				  if( closed == FALSE ) x=isomLRinv(xilr) else x=xilr
				  xNA=x[,1]
				  x1=x[,indM[i]]
				  x[,1]=x1
				  x[,indM[i]]=xNA
			  }
		}
	
		res <- list(xOrig=xcheck, xImp=x, criteria=criteria, iter=it, 
			    maxit=maxit, w=length(which(w)), wind=w)

	}
	
	class(res) <- "imp"
	invisible(res)
}

