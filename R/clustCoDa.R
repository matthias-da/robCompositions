

clustCoDa <- function(x, k=NULL, method="Mclust",
          scale = "robust", transformation = "isomLR",
          distMethod=NULL, qtclustsize=0.7, 
          iter.max=100, eps=0.1, vals = TRUE, 
          alt = NULL, coord=NULL, bic=NULL, verbose = TRUE){
  if( method == "fixmahal" && vals == TRUE ){
    stop("vals are not available with method fixmahal")
  }
  partitioning <- c("Mclust")
  agglomerative <- c("Ward")
  if(is.null(k) & method %in% partitioning) stop("provide the number of clusters")
  if(!is.null(distMethod)){
    if(distMethod == "Aitchison" & transformation %in% c("isomLR", "cenLR")) {
      stop("either apply a \nlog-ratio transformation or the Aitchison distance, \nnot both")
    }
  }
  if(is.null(distMethod)) distMethod = "euclidean"
  ## prepare
  "prepare" <- function(x, scaling="classical", transformation="cenLR"){
      ## ---------------------------------------------------------------------------
      ## x ... matrix or data frame
      ## scaling: - classical
      ##          - robust (median)
      ##          - onestep (onestep)
      ##          - nonescale
      ##          - logcentered
      ## transformation: - logarithm
      ##                 - boxcox (powers must be chosen)
      ##                 - bcOpt (with optimal powers)
      ##                 - logcentered
      ##                 - clr
      ##                 - ilr
      ## ---------------------------------------------------------------------------
      ### scaling:
      classical <- function(x){ scale(x) }
      robust <- function(x){ t(t((t(t(x) - apply(x, 2, median))))/apply(x,2,mad)) }
      onestep <- onestep <- function(x){
        mMedian <- median(x)
        mMad <- mad(x)
        constant <- 3/1.486
        limit1 <- mMedian + constant * mMad
        limit2 <- mMedian - constant * mMad
        w1 <- which( x > limit1 )
        w2 <- which( x < limit2 )
        x[w1] <- limit1
        x[w2] <- limit2
        mean(x)
      }
      robust2 <- function(x){ scale(x, center=apply(x, 3, onestep)) }
      nonescale <- function(x){ x }
      ### transformation:
      logarithm <- function(x){ log(x) }
      boxcox <- function(x, powers){ box.cox(x, powers) }
      bcOpt <- function(x){
        b <- box.cox.powers(x)$lambda
        box.cox(x, b)
      }
      nonetrans <- function(x){ x }
      logcentered <- function(x){ 
        xgeom=10^apply(log10(x),1,mean)
        x2=x/xgeom
        x2
      }
      res <- get(scaling)( get(transformation)( x ) )
      if(transformation == "cenLR") res <-  res$x.clr
      return(res)
  }
  xOrig <- x
  x <- prepare(x, scale, transformation)
#   
#   vp <- FALSE    # varplot information
  if( distMethod == "rf" ){
    cat("\n *** calculating random forest proximity measure...\n")
    flush.console()
    d <- sqrt( 1 - randomForest(x, proximity=TRUE)$proximity)
  }
  if( distMethod == "correlation" ){
    d <- 1 - abs(cor(t(x)))
  }
  if( distMethod == "robustCorrelation" ){
    d <- 1 - abs(covMcd(t(x), cor=TRUE)$cor)
  }  
  if( any(distMethod == c("gower", "bray", "kulczynski", "chord")) ) d <- gdist(x, method = distMethod)
  if( any(distMethod == c("morisita", "horn", "mountford")) ) d <- vegdist(x, method = distMethod)   
  menge1 <- c("gower", "bray", "kulczynski", "chord")
  menge2 <- c("morisita", "horn", "mountford")
  ##menge2 <- c(menge1, "euclidean", "rf", "cosadist")
  menge3 <- c("maximum", "canberra","euclidean", "manhattan")
  if(distMethod == "Aitchison") d <- aDist(x)
  if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
  if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
  if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)  
  
  
  findCenter <- function(x, clustering, k){
    a1 <- matrix(nrow=k, ncol=ncol(x))
    for( i in 1:k ){
      a1[i,] <- apply(x[ which( clustering == i ), , drop=FALSE ], 2, mean)
    }
    a1
  }
  
  clust <- list()
  if(verbose){
    cat(paste("\n *** running", method, "cluster algorithm...\n"))
    flush.console()
  }
  if( method == "kmeansHartigan" ) {
    a <- kmeans(d, k, algorithm="Hartigan-Wong", iter.max=iter.max, nstart=5 )
  }
  if( method == "kmeansLloyd" ) {
    a <- kmeans(d, k, algorithm="Lloyd", iter.max=iter.max, nstart=5 )   
  }
  if( method == "kmeansForgy" ) {
    a <- kmeans(d, k, algorithm="Forgy", iter.max=iter.max, nstart=5 )
  }
  if( method == "kmeansMacQueen" ) {
    a <- kmeans(d, k, algorithm="MacQueen", iter.max=iter.max, nstart=5 )
  }
  if( substr(method, 1, 6) == "kmeans"){
    clust$cluster <- a$cluster
    clust$centers <- a$centers
    clust$size <- a$size
  }
  if( method == "cmeansUfcl" ){
    a <- cmeans(d, k, method="ufcl")
    clust$cluster <- a$cluster
    clust$centers <- a$centers
    clust$membership <- a$membership
  }
  if( method == "pam"){
    a <- pam(d, k)
    clust$cluster <- a$cluster
    clust$center <- a$med
    clust$size <- a$clusinfo[,1]
  }
  if( method == "clara"){
    a <- pam(d, k)
    clust$cluster <- a$cluster
    clust$center <- a$med
    clust$size <- a$clusinfo[,1]
  }
  if( method == "fanny" ){
    a <- fanny(d, k)
    clust$cluster <- a$cluster
    clust$center <- a$med
    clust$size <- a$clusinfo[,1]
    clust$membership <- a$mem
  }
  if( method == "bclust" ){
    a <- bclust(d, k)
    clust$cluster <- a$cluster
    clust$center <- a$centers
    clust$size <- table(a$cluster)
  }
  if( method == "cmeans" || method == "cshell" ){
    a <- get(method)(d, k)
    clust$cluster <- a$cluster
    clust$centers <- a$centers
    clust$size <- a$size
    clust$membership <- a$membership
  }
  if( method == "Mclust" ){
 #   vp <- TRUE
    a <- Mclust(x, k)
    clust$cluster <- a$classification
    clust$center <- t(a$parameters$mean)
    clust$size <- table(a$classification)
    #clust$bic <- a$bic
    clust$BIC <- a$bic
    clust$model <- a$model
  }
  if( method == "kccaKmeans" ){
    a <- kcca(as.matrix(d), k, family=kccaFamily("kmeans"))
  }
  if( method == "kccaKmedians" ){
    a <- kcca(as.matrix(d), k, family=kccaFamily("kmedians"))
  }
  if( method == "kccaAngle" ){
    a <- kcca(as.matrix(d), k, family=kccaFamily("angle"))
  }
  if( method == "kccaJaccard" ){
    a <- kcca(as.matrix(d), k, family=kccaFamily("jaccard"))
  }
  if( method == "kccaEjaccard" ){
    a <- kcca(as.matrix(d), k, family=kccaFamily("ejaccard"))
  }
  if( substr(method, 1, 4) == "kcca"){
    clust$cluster <- a@cluster
    clust$center <- a@centers
    clust$size <- table(a@cluster)   
  }
  if( method == "speccRbfdot" ){
    a <- specc(as.matrix(d),centers=k)
  }
  if( method == "speccPolydot" ){
    a <- specc(as.matrix(d),centers=k, kernel="polydot")
  }   #  vanilladot tanhdot laplacedot besseldot anovadot splinedot
  if( method == "speccVanilladot" ){
    a <- specc(as.matrix(x),centers=k, kernel="vanilladot")
  }
  if( method == "speccTanhdot" ){
    a <- specc(as.matrix(x),centers=k, kernel="tanhdot")
  }
  if( method == "speccLaplacedot" ){
    a <- specc(as.matrix(x),centers=k, kernel="laplacedot")
  }
  if( method == "speccBesseldot" ){
    a <- specc(as.matrix(x),centers=k, kernel="besseldot")
  }
  if( method == "speccAnovadot" ){
    a <- specc(as.matrix(x),centers=k, kernel="anovadot")
  }
  if( method == "speccSplinedot" ){
    a <- specc(as.matrix(x),centers=k, kernel="splinedot")
  }
  if(substr(method, 1, 5) == "specc"){
    clust$cluster <- a@.Data
    clust$center <- a@centers
    clust$size <- a@size    
  }
  if( method == "hclustSingle" ){
    dtree <- hclust(d, method="single")
  }
  if( method == "hclustComplete" ){
    dtree <- hclust(d, method="complete")
  }
  if( method == "hclustAverage" ){
    dtree <- hclust(d, method="average")
  }
  if( method == "Ward" ){
    dtree <- hclust(d, method="ward")
  }
  if( method == "hclustMcquitty" ){
    dtree <- hclust(d, method="mcquitty")
  }
  if( method == "hclustMedian" ){
    dtree <- hclust(d, method="median")
  }
  if( method == "hclustcentroid" ){
    dtree <- hclust(d, method="centroid")
  }
  if(substr(method, 1, 6) == "hclust" | method == "Ward"){
    a <- cutree(dtree, k)
    clust$cluster <- as.numeric(a)
    clust$center <- findCenter(x, a, k)
    clust$size <- table(a)
  }
  ### validity: ---------------------------------------------------------------
  if(verbose){
    cat("\n *** calculating validity measure... \n")
    flush.console()
  }
  if( vals == FALSE ) clust.val <- m <- NA
  if( vals == TRUE && length(alt) > 1){
    if( length(coord) == 0 ){
      clust.val <- cluster.stats(dist(x), clust$cluster, as.numeric(alt))
    } else { clust.val <- cluster.stats(dist(coord), clust$cluster, as.numeric(alt)) }
    
    m <- data.frame( average.between = round(clust.val$average.between, 3), 
                     average.within = round(clust.val$average.within, 3),
                     avg.silwidth = round(clust.val$avg.silwidth, 3), 
                     hubertgamma = round(clust.val$hubertgamma, 3),
                     dunn = round(clust.val$dunn, 3), 
                     wb.ratio = round(clust.val$wb.ratio, 3),
                     corrected.rand = round(clust.val$corrected.rand, 3),
                     row.names = paste(method, "-", distMethod, sep=""))
    
  }
  if( vals == TRUE && length(alt) == 0){
    if( length(coord) == 0 ){
      clust.val <- cluster.stats(dist(x), clust$cluster)
    } else { clust.val <- cluster.stats(dist(coord), clust$cluster) }
    m <- data.frame( average.between = round(clust.val$average.between, 3), 
                     average.within = round(clust.val$average.within, 3),
                     avg.silwidth = round(clust.val$avg.silwidth, 3), 
                    # hubertgamma = round(clust.val$hubertgamma, 3),
                     dunn = round(clust.val$dunn, 3), 
                     wb.ratio = round(clust.val$wb.ratio, 3),
                     corrected.rand = NA,
                     row.names = paste(method, "-", distMethod, sep=""))
  }
  if( length(bic) > 0 ){
    cl <- Mclust(x,k,k)
    bics <- vector()
    for(i in 1:k ){
      bics[i] <- min(EMclust(x[cl$class==i,], 1), na.rm=TRUE)
    }
    ##m <- cbind(m,bics)
    clust$bic <- bics
  }
  clust$xdata <- x
  clust$method <- method
  clust$distMethod <- distMethod
  clust$k <- k
  if(substr(method, 1, 6) == "hclust" | method == "Ward"){
    clust$dtree <- dtree
  }
  clust$valTF <- vals
  if(vals){
    clust$average.between <- m$valMeasures$average.between
    clust$average.within <- m$valMeasures$average.within
    clust$avg.silwidth <- m$valMeasures$avg.silwith
    clust$dunn <- m$valMeasures$dunn
    clust$wb.ratio <- m$valMeasures$wb.ratio
    clust$corrected.rand <- m$valMeasures$corrected.rand
    clust$silwidths <- clust.val$clus.avg.silwidths
    clust$separation <- clust.val$separation
    clust$diameter <- clust.val$diameter
    clust$average.distance <- clust.val$average.distance
    clust$median.distance <- clust.val$median.distance
    clust$average.toother <- clust.val$average.toother
    #clust$bics <- clust$bics
    #clust$vp <- vp
  }
  class(clust) <- "clust"
  colnames(clust$center) <- colnames(x)
  class(clust) <- "clust"
  if(verbose) cat("\n finished\n")
  invisible(clust)
}

"plot.clust" <-
  function(x, y, ..., which = 2, val = "silwidths"){
    #coord, clust, k, val="silwidths", which.plot=c(1,2), Map="kola.background", texth=0.75){
    k <- x$k
    if(k < 13){r1 <- 3;r2 <- 4}
    if(k < 10){r1 <- 3;r2 <- 3}
    if(k < 7){r1 <- 2;r2 <- 3}
    if(k < 5){r1 <- 2;r2 <- 2}
    if(k < 3){r1 <- 1;r2 <- 2}
    if( which.plot == 1 ){   
      par(mfrow=c(r1,r2),pty="s",xaxt="n", yaxt="n", mar=c(0,0,0,0), omi=c(0,0,0.3,0),
          bg="white", fg="black")
      
      if( any( clust$method == c("cmeans", "cmeansUfcl")) ){
        ###g <- rep(0, 10)
        ###grey.col <- rep(0, 10)
        ###for( j in 1:10){
        ###  g[j] <- 2 * j / (10 * k)
        ###  grey.col[j] <- 1 - (j / 10)
        ###}
        for(i in 1:k){
          plot(coord, col=0, xlab="",ylab="")
          text(x=800000, y= 7870000, length(which(clust$cluster==i)))
          #title(paste(xname,"-",methodname,"cluster",i))
          ###for(j in 1:10){
          ###  points(coord[clust$mem[,i] > g[j], ],pch=15,col=gray(grey.col[j]))
          ### }
          points(coord, pch=15, col=gray(1-clust$mem[,i]))
          if( Map == "kola.background" ) plotKola.background(which.map=c(1,2,3,4),map.col=c(5,"grey","grey","grey"),map.lwd=c(1,1,1,1),
                                                             add.plot=TRUE)
        }
        #  legend( x=800000, y= 8000000, legend=g, col=c(gray(grey.col[1]), gray(grey.col[2]), gray(grey.col[3]), gray(grey.col[4]),
        #        gray(grey.col[5]), gray(grey.col[6]), gray(grey.col[7]), gray(grey.col[8]), gray(grey.col[9]), gray(grey.col[10])),
        #        pch=rep(15, 10) )
        vp1 <- viewport(x=0.5,y=0.5, w=1,height=1)
        pushViewport(vp1)
        if( clust$method != "Mclust" ){
          grid.text(x=0.5,y=0.98,
                    label=paste(clust$method, clust$distMethod, paste(names(clust$xdata), collapse=""), "Memberships" ))
        } else if( clust$method == "Mclust" ){     grid.text(x=0.5,y=0.98,
                                                             label=paste(clust$method, paste(names(clust$xdata), collapse=""), "Memberships" ))
        }
        popViewport()
        X11()
      }
      #X11()
      par(mfrow=c(r1,r2),pty="s",xaxt="n", yaxt="n", mar=c(0,0,0,0), omi=c(0,0,0.3,0),
          bg="white", fg="black")
      
      ###untere <- vals + abs(min(vals))+0.05
      ###fac1 <- 1/abs(max(untere))
      ###grays <- gray(1 - untere*fac1)
      if( all( vals > 0 ) ){
        grays <- gray(1 - vals/max(vals))
      } else {
        v <- vals
        v <- scale(vals)
        v <- v+abs(min(v))+0.05
        v2 <- 1 - v/max(v)
        v2[v2 > 0.9] <- 0.9
        grays <- gray(v2)
      }
      for(i in 1:k){
        plot(coord, col=gray(0.95), xlab="", ylab="means", pch=15)
        if( Map == "kola.background" ) plotKola.background(which.map=c(1,2,3,4),map.col=c(5,"grey","grey","grey"),map.lwd=c(1,1,1,1),
                                                           add.plot=TRUE)
        if(length(val) > 0){
          points(coord[clust$cluster==i,], pch=15, col=grays[i])
        } else { points(coord[clust$cluster==i,], pch=15, col=4) }
        text(x=800000, y= 7850000, paste("obs =",length(which(clust$cluster==i))))
        text(x=720000, y= 7890000, paste(val, "=", round(vals[i],2)))
        text(x=373951, y=7882172, i, cex=1.3)
      }
      vp1 <- viewport(x=0.5,y=0.5, w=1,height=1)
      pushViewport(vp1)
      if(clust$method != "Mclust"){
        #grid.text(x=0.5,y=0.98,
        #  label=paste(clust$method, clust$distMethod, paste(names(clust$xdata), collapse=""), val ))
        grid.text(x=0.5,y=0.965,
                  label=clust$method, gp=gpar(cex=1.3))     
      } else if( clust$method == "Mclust" ){
        #grid.text(x=0.5,y=0.98,
        #  label=paste(clust$method, paste(names(clust$xdata), collapse=""), val ))
        grid.text(x=0.5,y=0.965,
                  label="Mclust", gp=gpar(cex=1.3))
      }
      popViewport()
      pushViewport(vp1)
      if(clust$method != "Mclust"){
        #grid.text(x=0.5,y=0.98,
        #  label=paste(clust$method, clust$distMethod, paste(names(clust$xdata), collapse=""), val ))
        grid.text(x=0.5,y=0.965,
                  label=clust$method, gp=gpar(cex=1.3))     
      } else if( clust$method == "Mclust" ){
        #grid.text(x=0.5,y=0.98,
        #  label=paste(clust$method, paste(names(clust$xdata), collapse=""), val ))
        grid.text(x=0.5,y=0.965,
                  label="Mclust", gp=gpar(cex=1.3))
      }
      popViewport()
    }
    if( clust$vp == FALSE ) cat("\n --------- \n *** Note:\n elementplot is useless and not printed, \n because distances are used for clustering \n and not the data itself \n --------- \n")
    if( clust$vp == TRUE ){ yes <- TRUE } else { yes <- FALSE }
    if( all(which.plot==c(1,2)) && yes == TRUE ){ X11() }
    if( (all(which.plot==c(1,2)) && yes == TRUE) || (yes==TRUE & which.plot==2) ){
      ##cent <- matrix(clust$centers,ncol=k,byrow=T)
      # names of the variables
      rnam <- colnames(clust$center)
      # p ... Anzahl der Variablen
      # k ... Anzahl der Cluster
      p <- dim(clust$center)[2]
      #name=""
      for(j in 1:p){
        rnam[j] <- substring(rnam[j],1,2)
      }
      #######rownames(clust$centers) <- rnam
      ma <- max(abs(clust$center))
      # create the plot
      par(mfrow=c(1,1),cex=1,cex.axis=1,cex.lab=1.5,xaxt="s",yaxt="s")
      plot(clust$center[,1],type="n",xlim=range(0,1),ylim=range(-ma-0.3,ma+0.3),
           ylab="cluster means",xlab="", xaxt="n")
      segments(0, 0, 1, 0)
      #segments(0, 0.5, 1, 0.5, lty = 2)
      #segments(0, -0.5, 1, -0.5, lty = 2)
      if( clust$method == "Mclust" ){  } else{
        title(paste(clust$method, ",", clust$distMethod)) }
      bb <- c(0,1)
      bb1 <- bb/k
      ba <- seq(from = bb1[1], by = bb1[2])
      ba1 <- ba[2]/20
      ba2 <- c(0,ba1)
      segments(0,-ma,0,ma)
      for(i in 1:(k+1)){
        segments(ba[i],-ma,ba[i],ma)
      }
      # create weights
      weight <- 0
      for(i in 1:k){
        weight[i] <- clust$size[i]
      }
      sumweight <- sum(as.numeric(weight))
      # text
      for(j in 1:k){
        text(seq(from=ba[j]+ba[2]/p,to=ba[j+1]-ba[2]/p,
                 by=(ba[j+1]-ba[j]-2*ba[2]/p)/(p-1)), clust$center[j,], rnam,
             col="black",cex=texth)
        text(ba[j]+ba[2]/2,(ma+0.1)*(-1),paste(j,sep=""),col="black",cex=1.3)
        text(ba[j]+ba[2]/2,ma+0.3,paste(as.numeric(weight[j], sep="")),
             #substring(as.numeric(weight[j])/sumweight,1,4),sep=""),
             col=1,cex=1.2)
      }
      mtext("Number of observations for each cluster", cex=1.3)
      mtext("Cluster number", side=1, cex=1.3)
    }
  }


library(mclust)
library("mvoutlier")
library(robCompositions)
data("chorizon")
x <- chorizon[1:50, 101:110]

rr <- clustCoDa(x, k=6)
