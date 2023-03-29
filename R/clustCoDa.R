#' Cluster analysis for compositional data
#' 
#' Clustering in orthonormal coordinates or by using the Aitchison distance
#' 
#' The compositional data set is either internally represented by orthonormal coordiantes
#' before a cluster algorithm is applied, or - depending on the 
#' choice of parameters -  the Aitchison distance is used.
#' 
#' @aliases clustCoDa plot.clustCoDa 
#' @param x compositional data represented as a data.frame
#' @param k number of clusters
#' @param method clustering method. One of Mclust, cmeans, kmeansHartigan,
#' cmeansUfcl, pam, clara, fanny, ward.D2, single, hclustComplete, 
#' average, mcquitty, median, centroid
#' @param scale if orthonormal coordinates should be normalized.
#' @param transformation default are the isometric logratio coordinates. Can only used when distMethod 
#' is not Aitchison.
#' @param distMethod Distance measure to be used. If \dQuote{Aitchison}, then transformation should be \dQuote{identity}.
#' @param iter.max parameter if kmeans is chosen. The maximum number of iterations allowed 
#' @param vals if cluster validity measures should be calculated
#' @param alt a known partitioning can be provided (for special cluster validity measures)
#' @param bic if TRUE then the BIC criteria is evaluated for each single cluster as validity measure
#' @param verbose if TRUE additional print output is provided
#' @return all relevant information such as cluster centers, cluster memberships, and
#' cluster statistics.
#' @author Matthias Templ (accessing the basic features of hclust, Mclust, kmeans, etc. that 
#' are all written by others)
#' @references 
#' M. Templ, P. Filzmoser, C. Reimann.
#' Cluster analysis applied to regional geochemical data: Problems and possibilities. 
#' \emph{Applied Geochemistry}, \strong{23} (8), 2198--2213, 2008
#' @export
#' @references Templ, M., Filzmoser, P., Reimann, C. (2008) 
#' \emph{Cluster analysis applied to regional geochemical data: Problems and possibilities}, 
#' Applied Geochemistry, 23 (2008), pages 2198 - 2213.
#' 
#' @keywords multivariate
#' @importFrom stats hclust
#' @importFrom stats kmeans
#' @import mclust
#' @importFrom mclust Mclust 
#' @importFrom fpc cluster.stats
#' @importFrom cluster pam fanny 
#' @import e1071
#' @importFrom e1071 cmeans bclust
#' @importFrom kernlab specc
#' @importFrom mclust mclustBIC
#' @examples
#' data(expenditures)
#' x <- expenditures
#' rr <- clustCoDa(x, k=6, scale = "robust", transformation = "pivotCoord")
#' rr2 <- clustCoDa(x, k=6, distMethod = "Aitchison", scale = "none", 
#'                  transformation = "identity")
#' rr3 <- clustCoDa(x, k=6, distMethod = "Aitchison", method = "single",
#'                  transformation = "identity", scale = "none")
#'                  
#' \dontrun{
#' require(reshape2)
#' plot(rr)
#' plot(rr, normalized = TRUE)
#' plot(rr, normalized = TRUE, which.plot = "partMeans")
#' }
clustCoDa <- function(x, k=NULL, method="Mclust",
          scale = "robust", transformation = "pivotCoord",
          distMethod=NULL, iter.max=100, vals = TRUE, 
          alt = NULL, bic=NULL, verbose = TRUE){
  
  partitioning <- c("Mclust","cmeans","cshell","kmeansHartigan",
                    "kmeansLloyd","kmeansForgy","kmeansMacQueen",
                    "cmeansUfcl","pam","klara","fanny","blust",
                    "kccaKmeans","kccaKmedians","kccaAngle",
                    "speccRbfdot","speccVanilladot",
                    "speccTanhdot","speccLaplacedot","speccBesseldot",
                    "speccAnovadot","speccSplinedot")
  agglomerative <- c("ward.D2","single","complete","average",
                     "mcquitty","median",
                     "centroid")
  if(is.null(k) & method %in% partitioning) stop("provide the number of clusters")
  if(!is.null(distMethod)){
    if(distMethod == "Aitchison" & transformation %in% c("pivotCoord", "cenLR")) {
      stop("either apply a \nlog-ratio transformation or the Aitchison distance, \nnot both")
    }
  }
  if(is.null(distMethod) & !(transformation %in% c("pivotCoord", "cenLR"))) { 
     distMethod <- "Aitchison"
     message("Aitchison distance is used for the calculation\n of the distance matrix")
  }
  if(is.null(distMethod) & transformation %in% c("pivotCoord", "cenLR")) {
    distMethod <- "euclidean"
  }
  
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
      nonescale <- none <- identity <- function(x){ x }
      ### transformation:
      logarithm <- function(x){ log(x) }
      # boxcox <- function(x, powers){ car::bcPower(x, powers) }
      # bcOpt <- function(x){
      #   b <- car::powerTransform(x)$lambda
      #   car::bcPower(x, b)
      # }
      nonetrans <- identity <- none <- function(x){ x }
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
  
#   vp <- FALSE    # varplot information
  if( distMethod == "correlation" ){
    d <- 1 - abs(cor(x))
  }
  if( distMethod == "variation" ){
    d <- variation(x)
  }
  if( distMethod == "robustCorrelation" ){
    d <- 1 - abs(covMcd(x, cor=TRUE)$cor)
  }  
#  if( any(distMethod == c("gower", "bray", "kulczynski", "chord")) ) d <- gdist(x, method = distMethod)
#  if( any(distMethod == c("morisita", "horn", "mountford")) ) d <- vegdist(x, method = distMethod)   
#  menge1 <- c("gower", "bray", "kulczynski", "chord")
#  menge2 <- c("morisita", "horn", "mountford")
#  ##menge2 <- c(menge1, "euclidean", "rf", "cosadist")
#  menge3 <- c("maximum", "canberra","euclidean", "manhattan")
  if(distMethod == "Aitchison") d <- as.dist(aDist(x))
  if(distMethod == "euclidean" | distMethod == "Euclidean") d <- dist(x)
  if(distMethod == "manhattan" | distMethod == "Manhattan") d <- dist(x, method = "manhattan")
#  if( any(distMethod == menge3) ) d <- dist(x, method = distMethod )
#  if( any(distMethod == menge1) ) d <- gdist(x, method=distMethod)
#  if( any(distMethod == menge2) ) d <- vegdist(x, method=distMethod)  
  
  
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
  # if( method == "kccaKmeans" ){
  #   a <- kcca(as.matrix(d), k, family=kccaFamily("kmeans"))
  # }
  # if( method == "kccaKmedians" ){
  #   a <- kcca(as.matrix(d), k, family=kccaFamily("kmedians"))
  # }
  # if( method == "kccaAngle" ){
  #   a <- kcca(as.matrix(d), k, family=kccaFamily("angle"))
  # }
  # if( substr(method, 1, 4) == "kcca"){
  #   clust$cluster <- a@cluster
  #   clust$center <- a@centers
  #   clust$size <- table(a@cluster)   
  # }
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
  s <- c("single", "complete", "average", "ward.D2", "mcquitty", "median", "centroid")
  if( method %in% s){
    dtree <- hclust(d, method)
  }
  if(method %in% s){
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
    clust.val <- cluster.stats(as.dist(d), clust$cluster, as.numeric(alt))
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
    clust.val <- cluster.stats(as.dist(d), clust$cluster)
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
    browser()
    cl <- Mclust(x, G = k)
    bics <- vector()
    for(i in 1:k ){
      bics[i] <- min(mclustBIC(x[cl$class==i,], 1), na.rm=TRUE)
    }
    ##m <- cbind(m,bics)
    clust$bic <- bics
  }
#  clust$xdata <- x
  clust$method <- method
  clust$distMethod <- distMethod
  clust$k <- k
  if(substr(method, 1, 6) == "hclust" | method == "ward.D2"){
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
  if(distMethod != "Aitchison" & transformation == "pivotCoord"){
    gms <- apply(xOrig, 2, gm)
    clust$centerSimplex <- pivotCoordInv(clust$center)
    gmsc <- apply(clust$centerSimplex, 2, gm)
    di <- gms/gmsc
    # di <- gms/clust$centerSimplex
    for(i in 1:ncol(clust$centerSimplex)){
      clust$centerSimplex[, i] <- clust$centerSimplex[, i] * di[i]
    }
    colnames(clust$centerSimplex) <- colnames(xOrig)
  }
  if(distMethod == "Aitchison"){
    clust$centerSimplex <- clust$center
  }
  clust$transformation <- transformation
  clust$scaling <- scale
  class(clust) <- "clust"
  colnames(clust$center) <- colnames(x)
  class(clust) <- "clustCoDa"
  if(verbose) cat("\n finished\n")
  invisible(clust)
}

#' @rdname clustCoDa
#' @export
#' @method plot clustCoDa
#' @param y the y coordinates of points in the plot, optional if x is an appropriate structure.
#' @param ... additional parameters for print method passed through
#' @param normalized results gets normalized before plotting. Normalization is done by z-transformation 
#' applied on each variable.
#' @param which.plot currently the only plot. Plot of cluster centers.
#' @param measure cluster validity measure to be considered for which.plot equals \dQuote{partMeans}
plot.clustCoDa <- function(x, y, ..., 
                           normalized = FALSE, 
                           which.plot = "clusterMeans", 
                           measure = "silwidths"){
    variable <- center <- NULL
    if(x$transformation != "pivotCoord"){
      centers <- as.data.frame(x$center)
    } else {
      centers <- as.data.frame(x$centerSimplex)       
    }
    if( normalized ) centers <- scale(centers)
    #centers <- cbind("cluster" = rep(1:nrow(x$center), ncol(x$center)), centers)
    centers <- cbind("cluster" = 1:nrow(x$center), centers)
    centers <- reshape2::melt(centers, id = "cluster")
    colnames(centers) <- c("cluster", "variable", "center")
    centers <- centers[!centers$variable == "cluster", ]
    annotations <- paste(measure, "=", round(x[[measure]], 5))    
    xpos <- rep(2, length(annotations)) 
    ypos <- rep(Inf, length(annotations))
    cluster <- c(1:length(annotations))
    ldata <- data.frame(xpos, ypos, annotations, cluster)
    centers$cluster <- factor(centers$cluster)
    centers$measure <- rep(x[[measure]], length(unique(centers$variable)))
    centers$measure <- centers$measure + abs(min(centers$measure)) + abs(quantile(centers$measure, 0.1))
    centers$measure <- centers$measure / max(centers$measure)
    centers$measure <- grey(1 - centers$measure)
    if(which.plot == "partMeans"){  
      gg <- ggplot(centers, aes(x=variable, y=center, fill=measure)) + 
        geom_bar(stat = "identity", aes(fill=measure)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        facet_wrap(~cluster) + 
        geom_text(data = ldata, aes(x=length(annotations), y=max(centers$center), label=annotations),  
                  inherit.aes=FALSE, parse=FALSE) +
        scale_fill_identity()
      print(gg)
    }    
    if(which.plot == "clusterMeans"){  
      ggplot(centers, aes(x=cluster, y=center, fill=measure)) + geom_bar(stat = "identity") +
        facet_wrap(~variable) +         scale_fill_identity()
    }
}

# "plot.clust" <-
#   function(x, y, ..., which.plot = 2, val = "silwidths", coordinates = NULL){
#     #coord, clust, k, val="silwidths", which.plot=c(1,2), Map="kola.background", texth=0.75){
#     k <- x$k
#     if(is.null(coordinates)) coordinates <- x$coord
#     if(is.null(coordinates) & which.plot == 1){
#       stop("\n no coordinates/geographical information are/is provided.")
#     } 
#     if(k < 13){r1 <- 3;r2 <- 4}
#     if(k < 10){r1 <- 3;r2 <- 3}
#     if(k < 7){r1 <- 2;r2 <- 3}
#     if(k < 5){r1 <- 2;r2 <- 2}
#     if(k < 3){r1 <- 1;r2 <- 2}
#     if( which.plot == 1 & !is.null(coordinates)){   
#       par(mfrow=c(r1,r2), pty="s", xaxt="n", yaxt="n", mar=c(0,0,0,0), 
#           omi=c(0,0,0.3,0), bg="white", fg="black")
#       
#       if( any( x$method == c("cmeans", "cmeansUfcl", "fanny")) ){
#         ###g <- rep(0, 10)
#         ###grey.col <- rep(0, 10)
#         ###for( j in 1:10){
#         ###  g[j] <- 2 * j / (10 * k)
#         ###  grey.col[j] <- 1 - (j / 10)
#         ###}
#         for(i in 1:k){
#           plot(coord, col=0, xlab="", ylab="")
#           text(x=800000, y=7870000, length(which(x$cluster==i)))
#           #title(paste(xname,"-",methodname,"cluster",i))
#           ###for(j in 1:10){
#           ###  points(coord[x$mem[,i] > g[j], ],pch=15,col=gray(grey.col[j]))
#           ### }
#           points(coord, pch=15, col=gray(1 - x$mem[,i]))
# #          if( Map == "kola.background" ) plotKola.background(which.map=c(1,2,3,4),map.col=c(5,"grey","grey","grey"),map.lwd=c(1,1,1,1),
# #                                                             add.plot=TRUE)
#         }
#         #  legend( x=800000, y= 8000000, legend=g, col=c(gray(grey.col[1]), gray(grey.col[2]), gray(grey.col[3]), gray(grey.col[4]),
#         #        gray(grey.col[5]), gray(grey.col[6]), gray(grey.col[7]), gray(grey.col[8]), gray(grey.col[9]), gray(grey.col[10])),
#         #        pch=rep(15, 10) )
#         vp1 <- viewport(x=0.5,y=0.5, w=1,height=1)
#         pushViewport(vp1)
#         if( x$method != "Mclust" ){
#           grid.text(x=0.5,y=0.98,
#                     label=paste(x$method, x$distMethod, paste(names(x$xdata), 
#                                             collapse=""), "Memberships" ))
#         } else if( x$method == "Mclust" ){  grid.text(x=0.5,y=0.98,
#                                                              label=paste(x$method, paste(names(x$xdata), collapse=""), "Memberships" ))
#         }
#         popViewport()
#         X11()
#       }
#       #X11()
#       par(mfrow=c(r1,r2),pty="s",xaxt="n", yaxt="n", mar=c(0,0,0,0), omi=c(0,0,0.3,0),
#           bg="white", fg="black")
#       
#       ###untere <- vals + abs(min(vals))+0.05
#       ###fac1 <- 1/abs(max(untere))
#       ###grays <- gray(1 - untere*fac1)
#       if( all( vals > 0 ) ){
#         grays <- gray(1 - vals/max(vals))
#       } else {
#         v <- vals
#         v <- scale(vals)
#         v <- v+abs(min(v))+0.05
#         v2 <- 1 - v/max(v)
#         v2[v2 > 0.9] <- 0.9
#         grays <- gray(v2)
#       }
#       for(i in 1:k){
#         plot(coord, col=gray(0.95), xlab="", ylab="means", pch=15)
#         if( Map == "kola.background" ) plotKola.background(which.map=c(1,2,3,4),map.col=c(5,"grey","grey","grey"),map.lwd=c(1,1,1,1),
#                                                            add.plot=TRUE)
#         if(length(val) > 0){
#           points(coord[x$cluster==i,], pch=15, col=grays[i])
#         } else { points(coord[x$cluster==i,], pch=15, col=4) }
#         text(x=800000, y= 7850000, paste("obs =",length(which(x$cluster==i))))
#         text(x=720000, y= 7890000, paste(val, "=", round(vals[i],2)))
#         text(x=373951, y=7882172, i, cex=1.3)
#       }
#       vp1 <- viewport(x=0.5,y=0.5, w=1,height=1)
#       pushViewport(vp1)
#       if(x$method != "Mclust"){
#         #grid.text(x=0.5,y=0.98,
#         #  label=paste(x$method, x$distMethod, paste(names(x$xdata), collapse=""), val ))
#         grid.text(x=0.5,y=0.965,
#                   label=x$method, gp=gpar(cex=1.3))     
#       } else if( x$method == "Mclust" ){
#         #grid.text(x=0.5,y=0.98,
#         #  label=paste(x$method, paste(names(x$xdata), collapse=""), val ))
#         grid.text(x=0.5,y=0.965,
#                   label="Mclust", gp=gpar(cex=1.3))
#       }
#       popViewport()
#       pushViewport(vp1)
#       if(x$method != "Mclust"){
#         #grid.text(x=0.5,y=0.98,
#         #  label=paste(x$method, x$distMethod, paste(names(x$xdata), collapse=""), val ))
#         grid.text(x=0.5,y=0.965,
#                   label=x$method, gp=gpar(cex=1.3))     
#       } else if( x$method == "Mclust" ){
#         #grid.text(x=0.5,y=0.98,
#         #  label=paste(x$method, paste(names(x$xdata), collapse=""), val ))
#         grid.text(x=0.5,y=0.965,
#                   label="Mclust", gp=gpar(cex=1.3))
#       }
#       popViewport()
#     }
#     if( x$vp == FALSE ) cat("\n --------- \n *** Note:\n elementplot is useless and not printed, \n because distances are used for clustering \n and not the data itself \n --------- \n")
#     if( x$vp == TRUE ){ yes <- TRUE } else { yes <- FALSE }
#     if( all(which.plot==c(1,2)) && yes == TRUE ){ X11() }
#     if( (all(which.plot==c(1,2)) && yes == TRUE) || (yes==TRUE & which.plot==2) ){
#       ##cent <- matrix(x$centers,ncol=k,byrow=T)
#       # names of the variables
#       rnam <- colnames(x$center)
#       # p ... Anzahl der Variablen
#       # k ... Anzahl der Cluster
#       p <- dim(x$center)[2]
#       #name=""
#       for(j in 1:p){
#         rnam[j] <- substring(rnam[j],1,2)
#       }
#       #######rownames(x$centers) <- rnam
#       ma <- max(abs(x$center))
#       # create the plot
#       par(mfrow=c(1,1),cex=1,cex.axis=1,cex.lab=1.5,xaxt="s",yaxt="s")
#       plot(x$center[,1],type="n",xlim=range(0,1),ylim=range(-ma-0.3,ma+0.3),
#            ylab="cluster means",xlab="", xaxt="n")
#       segments(0, 0, 1, 0)
#       #segments(0, 0.5, 1, 0.5, lty = 2)
#       #segments(0, -0.5, 1, -0.5, lty = 2)
#       if( x$method == "Mclust" ){  } else{
#         title(paste(x$method, ",", x$distMethod)) }
#       bb <- c(0,1)
#       bb1 <- bb/k
#       ba <- seq(from = bb1[1], by = bb1[2])
#       ba1 <- ba[2]/20
#       ba2 <- c(0,ba1)
#       segments(0,-ma,0,ma)
#       for(i in 1:(k+1)){
#         segments(ba[i],-ma,ba[i],ma)
#       }
#       # create weights
#       weight <- 0
#       for(i in 1:k){
#         weight[i] <- x$size[i]
#       }
#       sumweight <- sum(as.numeric(weight))
#       # text
#       for(j in 1:k){
#         text(seq(from=ba[j]+ba[2]/p,to=ba[j+1]-ba[2]/p,
#                  by=(ba[j+1]-ba[j]-2*ba[2]/p)/(p-1)), x$center[j,], rnam,
#              col="black",cex=texth)
#         text(ba[j]+ba[2]/2,(ma+0.1)*(-1),paste(j,sep=""),col="black",cex=1.3)
#         text(ba[j]+ba[2]/2,ma+0.3,paste(as.numeric(weight[j], sep="")),
#              #substring(as.numeric(weight[j])/sumweight,1,4),sep=""),
#              col=1,cex=1.2)
#       }
#       mtext("Number of observations for each cluster", cex=1.3)
#       mtext("Cluster number", side=1, cex=1.3)
#     }




