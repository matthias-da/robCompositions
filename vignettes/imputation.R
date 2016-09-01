## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
concordance=TRUE
)

## ----load package, echo=FALSE, results='hide'----------------------------
library("robCompositions")
constSum <- function(x, const=1){
	x / rowSums(x) * const
}

## ----echo=FALSE, results='hide'------------------------------------------
##require(compositions)
genData <- function(n=1000, out=0.05, 
            Sigma=5*c(1,1)%*%t(c(1,1))+0.05*c(1,-1)%*%t(c(1,-1))){
    ## Gruppe ohne Ausreisser:
    z <- mvrnorm(n, mu=c(0,2), Sigma=Sigma)
    N <- dim(z)[1]
    n1 <- N - floor(n*out)
    n2 <- N - floor(2*n*out)
    if(out > 0){
      z[(n1+1):N, ] <- mvrnorm(floor(n*out), mu=c(0,6), Sigma=Sigma) ## erste Ausreissergruppe (Euclidean+Aitchison)
    }
    z <- isomLRinv(z) #ilr.inv(z)
    sum=runif(n1,0,1)  #rnorm(n1,10,1)
    z[1:n1, ] <- z[1:n1,] * sum
    if(out > 0){ 
      sum1=runif(floor(2*n*out),13,17) #rnorm(n2-n1,15,1)
      z[(n2+1):N, ] <- z[(n2+1):N, ] * sum1
      z[(n1+1):n2, ] <- z[(n1+1):n2, ] * 10
    }
    ## generate missings
    zmiss <- z
    s <- c(0.2, 0.1, 0.05, 0.05)
    for(i in 1:ncol(z)){
      zmiss[sample(1:n2, floor(s[i]*n2)), i] <- NA #1:index
    }
    list(zmiss=data.frame(zmiss), z2=data.frame(z), good=n2)
}

## ----echo=FALSE, results='hide'------------------------------------------
genData <- function(n=1000, out=0.05, 
            Sigma=1*c(1,1)%*%t(c(1,1))+0.05*c(1,-1)%*%t(c(1,-1))){
    ## Gruppe ohne Ausreisser:
    z <- mvrnorm(n, mu=c(0,0), Sigma=Sigma)
    N <- dim(z)[1]
    n1 <- N - floor(n*out)
    n2 <- N - floor(2*n*out)
    if(out > 0){
      z[(n1+1):N, ] <- mvrnorm(floor(n*out), mu=c(0,6), Sigma=Sigma) ## erste Ausreissergruppe (Euclidean+Aitchison)
    }
    z <- isomLRinv(z) #ilr.inv(z)
    sum=runif(n1,0,1)  #rnorm(n1,10,1)
    z[1:n1, ] <- z[1:n1,] * sum
    if(out > 0){ 
      sum1=runif(floor(2*n*out),13,17) #rnorm(n2-n1,15,1)
      z[(n2+1):N, ] <- z[(n2+1):N, ] * sum1
      z[(n1+1):n2, ] <- z[(n1+1):n2, ] * 10
    }
    ## generate missings
    zmiss <- z
    s <- c(0.2, 0.1, 0.05, 0.05)
    for(i in 1:ncol(z)){
      zmiss[sample(1:n2, floor(s[i]*n2)), i] <- NA #1:index
    }
    list(zmiss=data.frame(zmiss), z2=data.frame(z), good=n2)
}

## ----seed, echo=FALSE, message=FALSE, warning=FALSE----------------------
set.seed(123)
library("MASS")

## ----new data, echo=FALSE------------------------------------------------
x <- genData(100)

## ----plot.acomp, echo=FALSE----------------------------------------------
plot.acomp <- 
function (x, ..., labels = colnames(X), cn = colnames(X), aspanel = FALSE,
    id = FALSE, idlabs = NULL, idcol = 2, center = FALSE, scale = FALSE,
    pca = FALSE, col.pca = par("col"), margin = "acomp", add = FALSE,
    triangle = !add, col = par("col"),
    cexT=1.5, 
    adj=-1    
    )
{
    col <- unclass(col)
    X <- oneOrDataset(x)
    oX <- X
    s60 <- sin(pi/3)
    c60 <- cos(pi/3)
    if (NCOL(X) > 3) {
        if (margin == "rcomp")
            infkt <- function(x, y, ...) {
                plot.acomp(rcompmargin(X, d = c(gsi.mapfrom01(x),
                  gsi.mapfrom01(y)), pos = 1)[, c(3, 2, 1)],
                  ..., aspanel = TRUE, center = center, scale = scale,
                  col = col)
            }
        else if (margin == "acomp") {
            infkt <- function(x, y, ...) {
                plot.acomp(acompmargin(X, d = c(gsi.mapfrom01(x),
                  gsi.mapfrom01(y)), pos = 1)[, c(3, 2, 1)],
                  ..., aspanel = TRUE, center = center, scale = scale,
                  col = col)
            }
        }
        else {
            if (!is.numeric(margin))
                margin <- match(margin, colnames(X))
            fest <- X[, margin, drop = FALSE]
            X <- X[, -margin]
            infkt <- function(x, y, ...) {
                plot.acomp(acomp(cbind(X[, c(gsi.mapfrom01(y),
                  gsi.mapfrom01(x))], fest)), ..., aspanel = TRUE,
                  center = center, scale = scale, col = col)
            }
        }
        nn <- NCOL(X)
        if (add)
            gsi.add2pairs(sapply(1:NCOL(X), gsi.mapin01), infkt,
                ...)
        else gsi.pairs(sapply(1:NCOL(X), gsi.mapin01), labels = labels,
            panel = infkt, ...)
    }
    else {
        if (is.null(cn)) {
            cn <- c(expression(x[1]), expression(x[2]), expression(x[3]))
        }
        if (aspanel) {
            usr <- par("usr")
            on.exit(par(usr))
            par(usr = c(0, 1, 0, 1), pty = "s")
            lines(x = c(0, c60, 1, 0), y = c(0, s60, 0, 0))
            text(0, 0.2, cn[1], pos = 4, offset = 0.01, xpd = TRUE, cex=2)
            text(1, 0.2, cn[2], pos = 2, offset = 0.01, xpd = TRUE, cex=2)
            text(0.5, s60, cn[3], pos = 3, offset = 0.01, xpd = TRUE, cex=2)
        }
        else {
            if (!add) {
                usr <- par("pty")
                on.exit(par(usr))
                par(pty = "s")
                plot(x = c(0, c60, 1, 0), y = c(0, s60, 0, 0),
                  xlim = c(0, 1), ylim = c(0, 1), type = "n",
                  xlab = "", ylab = "", axes = FALSE)
                gsi.plots[[dev.cur()]] <<- NULL
            }
            if (triangle) {
                segments(x0 = c(0, 1, c60), y0 = c(0, 0, s60),
                  x1 = c(1, c60, 0), y1 = c(0, s60, 0))
                mtext(cn[1], side = 1, adj = 0, padj=adj, line = 1.5, cex=cexT)
                mtext(cn[2], side = 1, adj = 1, padj=adj , line = 1.5, cex=cexT)
                text(0.5, s60 * 1.03, cn[3], pos = 3, offset = 0.01,
                  xpd = TRUE, cex=cexT)
            }
        }
        X <- acomp(X, c(1, 2, 3))
        Y <- scale.acomp(X, center = center, scale = scale)
        gsi.setCoorInfo(mean = if (center)
            -mean(acomp(X))
        else acomp(c(1, 1, 1)), scale = if (scale)
            1/msd(X)
        else 1)
        x <- Y[, 2] + Y[, 3] * c60
        y <- Y[, 3] * s60
        points(x, y, ..., col = col)
    }
    return(invisible(NULL))
}

## ----knn, message=FALSE, warning=FALSE-----------------------------------
library("robCompositions") 
packageDescription("robCompositions")$Version
xImp <- impKNNa(x$zmiss, k=6)

## ----class---------------------------------------------------------------
class(xImp)

## ----printSummary--------------------------------------------------------
methods(class = "imp")
xImp

