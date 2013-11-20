# `impRZilr` <-
# function(x, maxit=10, eps=0.1, method="roundedZero", 
# 		dl=rep(0.05, ncol(x)), bruteforce=FALSE){
# 
# 	`ilrM` <-
# 			function(x, info=TRUE){
# 		x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
# 		D=ncol(x)
# 		for (i in 1:ncol(x.ilr)){
# 			x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
# 		} 
# #		invisible(-x.ilr)
#         if(info)  {res <- list(xilr=-x.ilr,
# 			 xOrig=x)
# 	         class(res) <- "ilrTransform"
# 	    } else {
# 			res <- -x.ilr
# 		}
# 		res
# 	}
# 	`invilrM` <-
# 			function(x.ilr){
# 		if(class(x.ilr) =="ilrTransform" ){
# 			fac <- rowSums(x.ilr$xOrig)
# 			x.ilr <- x.ilr$xilr
# 			y <- matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
# 			D=ncol(x.ilr)+1
# 			y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
# 			for (i in 2:ncol(y)){
# 				for (j in 1:(i-1)){
# 					y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
# 				}
# 			}
# 			for (i in 2:(ncol(y)-1)){
# 				y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
# 			}
# 			yexp=exp(-y)
# 			x.back=yexp/apply(yexp,1,sum) * fac # * rowSums(derOriginaldaten)
# 			invisible(x.back)			
# 		} else {
# 			y=matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
# 			D=ncol(x.ilr)+1
# 			y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
# 			for (i in 2:ncol(y)){
# 				for (j in 1:(i-1)){
# 					y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
# 				}
# 			}
# 			for (i in 2:(ncol(y)-1)){
# 				y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
# 			}
# 			yexp=exp(-y)
# 			x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
# 			invisible(x.back)
#         #return(yexp)
# 		}
# 		x.back
# 	}
# 	
# 	
# 	
# 	if( is.vector(x) ) stop("x must be a matrix or data frame")
# 	stopifnot((method %in% c("ltsReg", "ltsReg2", "classical", "lm", "roundedZero","roundedZeroRobust")))
#     if( length(dl) < ncol(x)) stop(paste("dl has to be a vector of ", ncol(x)))
# 	
# 
# 	## zeros to NA:
# 	x[x==0] <- NA
# 
# 	##index of missings / non-missings
# 	w <- is.na(x)
# 	wn <- !is.na(x)
# 	w2 <- apply(x, 1, function(x){
#           length(which(is.na(x)))
# 	})
# 
# 
# 	##sort the columns of the data according to the amount of missings in the variables
# 	wcol <- apply(x,2,function(x) length(which(is.na(x))))
# 	indM <- sort(wcol, index.return=TRUE, decreasing=TRUE)$ix
# 	cn <- colnames(x)
# 	xcheck <- x
# 
# 	## initialisation:
# #		x[is.na(x)] <- 0.001
# 	    for(i in 1:length(dl)){
# 		   ind <- is.na(x[,i])
# 		   #PF# if(length(ind) > 0) x[ind,i] <- dl[i]/3*2 
# 		   if(length(ind) > 0) x[ind,i] <- dl[i]*runif(sum(ind),1/3,2/3)
# 	    }
# 		
# #		x <- constSum(x)
# 		
#     ## parameters:
# 		it=0
# 		criteria <- 10000000
# 		error <- rep(0, ncol(x))
# 		
# 	###########################################
# 	###  start the iteration
# 	while(it <= maxit & criteria >= eps){
#   		xold <- x
#   		it=it+1
#   		for(i in 1:ncol(x)){
# 		    ## change the first column with that one with the highest amount of NAs
# 		    ## in the step
# 			if(wcol[indM[i]] > 0){
# 		    xNA=x[,indM[i]]
# 		    x1=x[,1]
# 		    x[,1]=xNA
# 		    x[,indM[i]]=x1
# 			
# 			if(method == "roundedZero"){
# 				xilr <- ilrM(x)
# 				phi <- ilrM(cbind(rep(dl[indM[i]], nrow(x)), x[,-1,drop=FALSE]), info=FALSE)[,1] # TODO: phi auserhalb der Schleife!
# 				## --> x hat sich geaendert aber dl nicht.
# 				xilr2 <- data.frame(xilr$xilr)
# 				c1 <- colnames(xilr2)[1]
# 				colnames(xilr2)[1] <- "V1"
# 				reg1 = lm(V1 ~ ., data=xilr2)
# 				yhat2 <- predict(reg1, new.data=xilr2[,-i]) 	
# 				if(bruteforce){ 
# 					xilr2[w[, indM[i]], 1] <- ifelse(yhat2[w[, indM[i]]] <= phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2[w[, indM[i]]] )
# 				} else {
# #					s <- sd(reg1$res, na.rm=TRUE)
# 					s <- sqrt(sum(reg1$res^2)/(nrow(xilr2)-ncol(xilr2)))
# 					ex <- (phi - yhat2)/s 
# #####################################################
# # CHANGED PF:
# #PF#                                    if(all(dnorm(ex) > 5 * .Machine$double.eps)) yhat2 <- yhat2 - s*dnorm(ex)/pnorm(ex)
#                                         yhat2sel <- ifelse(dnorm(ex[w[, indM[i]]]) > .Machine$double.eps,
#                                                            yhat2[w[, indM[i]]] - s*dnorm(ex[w[, indM[i]]])/pnorm(ex[w[, indM[i]]]),
#                                                            yhat2[w[, indM[i]]])
#                                         if(any(is.na(yhat2)) || any(yhat2=="Inf")) stop("Problems in ilr because of infinite or NA estimates")
#                                         # check if we are under the DL:
#                                         if(any(yhat2sel >= phi[w[, indM[i]]])){
# 						yhat2sel <- ifelse(yhat2sel > phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2sel)
# 					}
#                                         xilr2[w[, indM[i]], 1] <- yhat2sel
# #####################################################
# 		        }
# 			}
# 			if(method == "roundedZeroRobust"){
# 				xilr <- ilrM(x)
# 				x[x < .Machine$double.eps] <- 0.00000000001  ## TODO: better solution 
# 				phi <- ilrM(cbind(rep(dl[indM[i]], nrow(x)), x[,-1,drop=FALSE]), info=FALSE)[,1] # TODO: phi auserhalb der Schleife!
# 				xilr2 <- data.frame(xilr$xilr)
# 				c1 <- colnames(xilr2)[1]
# 				colnames(xilr2)[1] <- "V1"
# 				reg1 = rlm(V1 ~ ., data=xilr2, method="MM",maxit = 100)
# #	            reg1 = lmrob(V1 ~ ., data=xilr2)
# 				yhat2 <- predict(reg1, new.data=xilr2[,-i]) 	
# 				if(bruteforce){ 
# 					xilr2[w[, indM[i]], 1] <- ifelse(yhat2[w[, indM[i]]] <= phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2[w[, indM[i]]] )
# 				} else {
# #						s <- mad(reg1$res, na.rm=TRUE)
# 					s <- reg1$s
# 					#PF# s <- IQR(reg1$resid)/1.349
# 					ex <- (phi - yhat2)/s 
# #####################################################
# # CHANGED PF:
# #PF#					if(all(dnorm(ex) > 5 * .Machine$double.eps)) yhat2 <- yhat2 - s*dnorm(ex)/pnorm(ex)
# 					yhat2sel <- ifelse(dnorm(ex[w[, indM[i]]]) > .Machine$double.eps,
# 					                   yhat2[w[, indM[i]]] - s*dnorm(ex[w[, indM[i]]])/pnorm(ex[w[, indM[i]]]), 
# 					                   yhat2[w[, indM[i]]])
# 					if(any(is.na(yhat2)) || any(yhat2=="Inf")) stop("Problems in ilr because of infinite or NA estimates")
# 					# check if we are under the DL:
#                                         if(any(yhat2sel >= phi[w[, indM[i]]])){
# 						yhat2sel <- ifelse(yhat2sel > phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2sel)
# 					}
# 					xilr2[w[, indM[i]], 1] <- yhat2sel
# #####################################################
# 				}
# 			}
# 			
# 			xilr$xilr <- xilr2 
# 			x=invilrM(xilr)		
# 			## return the order of columns:
# 			xNA=x[,1]
# 			x1=x[,indM[i]]
# 			x[,1]=x1
# 			x[,indM[i]]=xNA
# 			}
# 
# 
#  	   }
# 
# 
#   	  criteria <- sum( ((xold - x)/x)^2, na.rm=TRUE) ## DIRTY: (na.rm=TRUE)
# 	  colnames(x) <- colnames(xcheck)
# 
# 	}
# 		
# 	res <- list(xOrig=xcheck, xImp=x, criteria=criteria, iter=it, 
# 			    maxit=maxit, w=length(which(w)), wind=w)
# 	class(res) <- "imp"
# 	invisible(res)
# }
# 
