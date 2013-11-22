plot.imp <-
function (x, ..., which=1, ord=1:ncol(x),
          colcomb = "missnonmiss", plotvars = NULL,
          col = c("skyblue", "red"), alpha = NULL,
          lty = par("lty"), xaxt = "s", 
          xaxlabels = NULL,
          las = 3, interactive = TRUE,  pch = c(1, 3),
          smooth = FALSE, reg.line = FALSE,
          legend.plot = FALSE,  
          ask = prod(par("mfcol")) < 
            length(which) && dev.interactive(), 
          center = FALSE, 
          scale=FALSE, id=FALSE,
          seg.l=0.02, seg1=TRUE)
{
	if (class(x) != "imp") 
		stop("use only with objects of class \"imp\" ")
	wind <- x$wind
	x <- x$xImp  
  if (!is.numeric(which) || any(which < 1) || any(which > 3)) 
		stop("'which' must be in 1:3")
  if(length(ord) != ncol(x)) stop(paste("'ord' is a vector of length", length(ord), 
					  "\n but has to be of length", ncol(x)))
  if (ncol(x) < 2) 
	  stop("'x' must be at least 2-dimensional.")
  if (length(colcomb) == 0){ 
	  stop("'colcomb' must have positive length.")
  }	else if(length(colcomb) > ncol(x)){ 
	  stop("'colcomb' is too long.")
  }
  if (length(col) != 2) 
	  stop("'col' must be a vector of length 2.")
  if (length(lty) == 1){
	  lty <- rep(lty, 2)
  } else if(length(lty) != 2){ 
	  stop("'lty' must be a vector of length 1 or 2.")
  }
  if (length(pch) == 1){
	  pch <- rep(pch, 2)
  } else if(length(pch) != 2){ 
	  stop("'pch' must be a vector of length 1 or 2.")
  }
  if(class(seg1) != "logical") stop("'seg1' must be logical")
  if(length(seg1) != 1) stop("'seg1' has to be a vector of length 1") 
  if(length(seg.l) != 1) stop("'seg.l' has to be a vector of length 1") 
  x <- as.data.frame(x)
  x <- x[, ord]
  wind <- wind[, ord]
  show <- rep(FALSE, 3)
  show[which] <- TRUE
  if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
  }
  #######################################################################
  ###   tenary plot:
  if(show[3]){
	 if(ncol(x) > 3){ 
		 warning(paste("tenary plot is only visible for 3 variables, you have", 
						 ncol(x),"\n only the first three variables are selected for plotting"))
	     x <- x[,1:3]
		 wind <- wind[,1:3]
	 }
	 if(ncol(x) == 2){ warning("tenary plot is only defined for 3 variables, 
                                \n you have 2")}
	 
	 col <- unclass(col)
	   cn <- colnames(x)
	   s60 <- sin(pi/3)
	   c60 <- cos(pi/3)
	   opar <- par(mar=c(3,2,2,0.5))
       plot(x = c(0, c60, 1, 0), y = c(0, s60, 0, 0), 
			 xlim = c(0, 1), ylim = c(0, 1), type = "n", 
			 xlab = "", ylab = "", axes = FALSE)
		segments(x0 = c(0, 1, c60), y0 = c(0, 0, s60), 
				 x1 = c(1, c60, 0), y1 = c(0, s60, 0))
		mtext(cn[1], side = 1, adj = 0, padj=-2, line = 1.5, cex=1.2)
		mtext(cn[2], side = 1, adj = 1, padj=-2, line = 1.5, cex=1.2)
		text(0.5, s60 * 1.05, cn[3], pos = 3, offset = 0.01, xpd = TRUE, cex=1.2)
		X <- constSum(x, c(1, 2, 3))
		Y <- scale(constSum(X), center = center, scale = scale)
		x2 <- Y[, 2] + Y[, 3] * c60
		y2 <- Y[, 3] * s60
		ind <- apply(wind, 1, any)
		points(x2, y2, col = col[1], pch=pch[1], cex=1)
		points(x2[ind], y2[ind], col = col[2], pch=pch[1], cex=1)
		n <- length(x2)
			ind <- apply(wind, 1, any)
			ind1 <- which(wind[,1])
			ind2 <- which(wind[,2])
			ind3 <- which(wind[,3])
			info <- apply(wind, 2, any)
			## first direction:
			seg.col="red"
			seg.lwd=1
			dir <- function(x,y, k=0.025){
					if(length(x) < 1){ 
						z1s <- z2s <- z3s <- matrix(0, nrow=length(x), ncol=2)
					} else{
						z1s <- z2s <- z3s <- matrix(0, nrow=length(x), ncol=2)
						for(i in 1:length(x)){
						  alpha <- atan(y[i]/x[i])
						  alphaz <- atan(x[i]/y[i])
						  k1s <- k*cos(alpha)
						  k2s <- k*sin(alpha)
						  z1s[i,] <- c(x[i], y[i]) + c(-k*cos(alpha), -k*sin(alpha))
						  z2s[i,] <- c(x[i], y[i]) + c(k*cos(alpha), -k*sin(alpha))
						  z3s[i,] <- if(x[i] < 0.5){ 
							  c(x[i], y[i]) + c(cos(alphaz)*k, sin(alphaz)*k)
						  } else{ 
							  c(x[i], y[i]) + c(-cos(alphaz)*k, sin(alphaz)*k)
						  }
						}
					}
					list(z1=z1s, z2=z2s, z3=z3s)
			}
			dir2 <- function(x,y,k=0.025){
				z1s <- z2s <- z3s <- matrix(0, nrow=length(x), ncol=2)
				z3s <- cbind(x,y) 
				z3s[,2] <- z3s[,2] + k
				z1s <- cbind(x,y) 
				z1s[,1] <- z1s[,1] - k*cos(0.45)
				z1s[,2] <- z1s[,2] - k*sin(0.45)				
				z2s <- cbind(x,y) 
				z2s[,1] <- z2s[,1] + k*cos(0.45)
				z2s[,2] <- z2s[,2] - k*sin(0.45)			
				list(z1=z1s, z2=z2s, z3=z3s)				
			}
			if(seg1) d <- dir(x=x2, y=y2, k=seg.l) else d <- dir2(x=x2, y=y2, k=seg.l)
			
			segments(x1=d$z1[,1], y1=d$z1[,2],
					x0=x2, y0=y2,
					col=col[1], lwd=seg.lwd)	
			segments(x1=d$z2[,1], y1=d$z2[,2],
					x0=x2, y0=y2,
					col=col[1], lwd=seg.lwd)				
			segments(x1=d$z3[,1], y1=d$z3[,2],
							x0=x2, y0=y2,
							col=col[1], lwd=seg.lwd)	
					
			if(info[1]){		
					segments(x1=d$z1[ind1,1], y1=d$z1[ind1,2],
							x0=x2[ind1], y0=y2[ind1],
							col=col[2], lwd=seg.lwd+2)	
				}
			if(info[2]){
					segments(x1=d$z2[ind2,1], y1=d$z2[ind2,2],
							x0=x2[ind2], y0=y2[ind2],
							col=col[2], lwd=seg.lwd+2)	
				}
			if(info[3]){
					segments(x1=d$z3[ind3,1], y1=d$z3[ind3,2],
							x0=x2[ind3], y0=y2[ind3],
							col=col[2], lwd=seg.lwd+2)	
				}
							
		if (id) {
				 if (is.null(idlabs)) 
					 idlabs <- paste(cn[1], "=", round(X[, 1], 2), 
							 ",\n", cn[2], "=", round(X[, 2], 2), ",\n", 
							 cn[3], "=", round(X[, 3], 2))
				 return(identify(x, idlabs, xpd = NA))
		}
		par(opar)
		 return(invisible(NULL))	  
  }

  #######################################################################
  ###   scatterplot matrix
  if(show[1]){
    if (!(inherits(x, c("data.frame", "matrix"))))
        stop("'x' must be a data.frame or matrix.")
    if (ncol(x) < 2)
        stop("'x' must be at least 2-dimensional.")
    if (length(col) != 2)
        stop("'col' must be a vector of length 2.")
    if (length(pch) != 2)
        stop("'pch' must be a vector of length 2.")
    if (!is.null(alpha))
        col <- alphablend(col, alpha)
    x <- as.data.frame(x)
    ind <- which(!unlist(lapply(x, is.numeric)))
    x[, ind] <- sapply(ind, function(i, x) as.numeric(x[, i]),
        x)
    args <- list(smooth = smooth, reg.line = reg.line, legend.plot = legend.plot,
        ...)
    if (!is.null(args$groups)) {
        args$groups <- FALSE
        warning("'groups' will be ignored.")
    }
    if (any(wind)) {
        w <- apply(wind, 2, any)
        #if (length(which(!w)) < 2)
  		#stop("at least two variables without missings are required.")
        args$x <- x
        #navec <- if (colcomb[1] == "missnonmiss")
        navec <- apply(wind, 1, any)
        #else apply(wind[, colcomb, drop = FALSE], 1, any)
        if (all(navec)) {
            args <- c(args, list(col = col, pch = pch[2]))
        }
        else {
            colvec <- ifelse(navec, col[2], col[1])
            args <- c(args, list(col = c(col[1], col), pch = pch,
                groups = as.factor(colvec)))
        }
    }
    else {
        args <- c(list(x = x, col = rep(col[1], 2),
            pch = pch[1]),
            args)
    }
    args$col <- c(args$col[1], args$col[3], args$col[2])
    args$pch <- args$pch[2:1]
    do.call(scatterplotMatrix, args)
    invisible()
  }
  #########################################################################
  ###   parallel coordinate plot
  if(show[2]){
    if (!(inherits(x, c("data.frame", "matrix"))))
        stop("'x' must be a data.frame or matrix.")
    if (ncol(x) < 2)
        stop("'x' must be at least 2-dimensional.")
    if (length(colcomb) == 0)
        stop("'colcomb' must have positive length.")
    else if (length(colcomb) > ncol(x))
        stop("'colcomb' is too long.")
    if (length(col) != 2)
        stop("'col' must be a vector of length 2.")
    if (length(lty) == 1)
        lty <- rep(lty, 2)
    else if (length(lty) != 2)
        stop("'lty' must be a vector of length 1 or 2.")
    x <- as.data.frame(x)
    ind <- which(!unlist(lapply(x, is.numeric)))
    x[, ind] <- sapply(ind, function(i, x) as.numeric(x[, i]),
        x)
    if (!is.null(alpha))
        col <- alphablend(col, alpha)
    createPlot <- function() {
        op <- par(xaxt = xaxt, las = las)
        on.exit(par(op))
        z <- if (is.null(plotvars))
            x
        else x[, plotvars]
        if (any(wind) && length(colcomb)) {
            navec <- if (colcomb[1] == "missnonmiss"){
                apply(wind, 1, any)
              } else{ apply(wind[, colcomb, drop = FALSE], 1, any)}
            colvec <- ifelse(navec, col[2], col[1])
            ltyvec <- ifelse(navec, lty[2], lty[1])
            ord <- order(colvec, decreasing = col[2] < col[1])
            z <- z[ord, ]
            cols <- colvec[ord]
            ltys <- ltyvec[ord]
        }
        else {
            cols <- col[1]
            ltys <- lty[1]
        }
        if (!is.null(xaxlabels))
            colnames(z) <- xaxlabels
        parcoord(z, cols, ltys, ...)
    }
    createPlot()
    interactiveDevices <- c("X11", "quartz", "windows")
    dev <- names(dev.cur())
    if (interactive && any(!is.na(charmatch(interactiveDevices,
        dev)))) {
        cat(paste("\nClick on a coordinate axis to highlight",
            "missings of the corresponding variable.\n"))
        cat(paste("To regain use of the VIM GUI and the R console,",
            "click outside the plot region.\n\n"))
        if (is.null(plotvars)) {
            p <- ncol(x)
            plotvars <- colnames(x)
        }
        else {
            p <- length(plotvars)
            plotvars <- colnames(x)[plotvars]
        }
        pt <- locator(1)
        colcomb <- character()
        while (!is.null(pt) && 1 <= pt$x && pt$x < p && 0 <=
            pt$y && pt$y <= 1) {
            i <- round(pt$x)
            colcomb <- if (plotvars[i] %in% colcomb)
                setdiff(colcomb, plotvars[i])
            else c(colcomb, plotvars[i])
            createPlot()
            if (length(colcomb) == 0) {
                cat("No missings highlighted.")
            }
            else if (length(colcomb) == 1) {
                cat(paste("Highlighted imputed values in variable ",
                  colcomb, ".\n", sep = "'"))
            }
            else {
                colcombout <- paste(colcomb, collapse = "', '")
                cat(paste("Highlighted imputed values in variables ",
                  colcombout, ".\n", sep = "'"))
            }
            flush.console()
            pt <- locator(1)
        }
    }
  }
    invisible()
}