#' imputeUDLs
#' 
#' @title Imputation of values above an upper detection limit in compositional data
#' 
#' @details An imputation method for right-censored compositional data. 
#' Statistical analysis is not possible with values reported in data, 
#' for example as ">10000". These values are replaced using tobit regression.
#' 
#' The algorithm iteratively imputes parts with values above upper detection limit
#' whereas in each step (1) compositional data are expressed in pivot coordinates (2) tobit regression is
#' applied (3) the values above upper detection limit are replaced by the expected values (4) the
#' corresponding inverse ilr mapping is applied. After all parts are
#' imputed, the algorithm starts again until the imputations only change marginally.
#' 
#' @description Parametric replacement of values above upper detection limit for compositional data using
#' classical and robust methods (possibly also the pls method) based on ilr-transformations with special
#' choice of balances. 
#' 
#' @param x data.frame or matrix
#' @param maxit maximum number of iterations
#' @param eps convergency criteria 
#' @param method either "lm", "lmrob" or "pls"
#' @param dl Detection limit for each variable. zero for variables with
#' variables that have no detection limit problems.
#' @param variation, if TRUE those predictors are chosen in each step, who's variation is lowest to the predictor.
#' @param nPred, if determined and variation equals TRUE, it fixes the number of predictors 
#' @param nComp if determined, it fixes the number of pls components. If
#' \dQuote{boot}, the number of pls components are estimated using a
#' bootstraped cross validation approach.
#' @param bruteforce sets imputed values above the detection limit to the
#' detection limit. Replacement above the detection limit are only exeptionally
#' occur due to numerical instabilities. The default is FALSE!
#' @param noisemethod adding noise to imputed values. Experimental
#' @param noise TRUE to activate noise (experimental)
#' @param R number of bootstrap samples for the determination of pls
#' components. Only important for method \dQuote{pls}.
#' @param correction normal or density
#' @param verbose additional print output during calculations.
#' @importFrom cvTools cvFit 
#' @importFrom zCompositions multRepl
#' @importFrom fpc pamk
#' @return 
#' \item{x }{imputed data} \item{criteria }{change between last and
#' second last iteration} \item{iter }{number of iterations} \item{maxit
#' }{maximum number of iterations} \item{wind}{index of values above upper detection limit}
#' \item{nComp}{number of components for method pls} \item{method}{chosen
#'   method}
#' @author Peter Filzmoser, Dominika Miksova based on function imputeBDLs code from Matthias Templ
#' @references 
#' Martin-Fernandez, J.A.,  Hron K.,  Templ, M., Filzmoser, P.  and Palarea-Albaladejo, J. (2012).
#' Model-based replacement of rounded zeros in compositional data:  Classical and robust approaches.
#' \emph{Computational Statistics and Data Analysis}, 56, 2688-2704.
#' 
#' Templ, M. and Hron, K. and Filzmoser and Gardlo, A. (2016). 
#' Imputation of rounded zeros for high-dimensional compositional data. 
#' \emph{Chemometrics and Intelligent Laboratory Systems}, 155, 183-190.
#' 
#' @seealso \code{\link{imputeBDLs}}
#' @keywords manip multivariate
#' @export
#' @importFrom MASS rlm
#' @examples
#' data(gemas)  # read data
#' dat <- gemas[gemas$COUNTRY=="HEL",c(12:29)]
#' UDL <- apply(dat,2,max)
#' names(UDL) <- names(dat)
#' UDL["Mn"] <- quantile(dat[,"Mn"], probs = 0.8)  # UDL present only in one variable
#' whichudl <- dat[,"Mn"] > UDL["Mn"] 
#' # classical method
#' imp.lm <- dat
#' imp.lm[whichudl,"Mn"] <- Inf
#' res.lm <- imputeUDLs(imp.lm, dl=UDL, method="lm", variation=TRUE)
#' imp.lm <- res.lm$x
#' 
#' 
imputeUDLs <-
  function (x, maxit = 10, eps = 0.1, method = "lm", dl = NULL, 
            variation = TRUE, nPred = NULL, nComp = "boot", bruteforce = FALSE, 
            noisemethod = "residuals", noise = FALSE, R = 10, correction = "normal", 
            verbose = FALSE) 
  {
    isomLRInvp <- 1
    isomLRinv <- function(x) {
      pivotCoordInv(x = x)
    }
    stopifnot((method %in% c("lm", "MM", "lmrob", "pls")))
    if (is.null(dl)) {
      is.na(x) <- sapply(x, is.infinite)
      dl <- apply(x, 2, max, na.rm = TRUE)
    }
    if (method == "pls" & ncol(x) < 5) 
      stop("too few variables/parts for method pls")
    if (!(correction %in% c("normal", "density"))) {
      stop("correction method must be normal or density")
    }
    if (method == "pls" & variation) {
      stop("if variation is TRUE then pls is not supported.")
    }
    if (is.null(nComp)) {
      pre <- FALSE
      nC <- NULL
    }
    else if (nComp == "boot") {
      nC <- integer(ncol(x))
      pre <- TRUE
    }
    else if (length(nComp) == ncol(x)) {
      nC <- nComp
      pre <- FALSE
    }
    else {
      pre <- FALSE
    }
    n <- nrow(x)
    d <- ncol(x)
    rs <- rowSums(x, na.rm = TRUE)
    x[x == Inf] <- NA
    indexFinalCheck <- is.na(x)
    cn <- colnames(x)
    wcol <- -abs(apply(x, 2, function(x) sum(is.na(x))))
    o <- order(wcol)
    x <- x[, o]
    if (verbose) 
      cat("number of variables > UDL:\n", sum(wcol != 0))
    dlordered <- dl[o]
    w <- is.na(x)
    wn <- !is.na(x)
    xcheck <- x
    w2 <- is.na(x)
    indNA <- apply(x, 2, function(x) {
      any(is.na(x))
    })
    for (i in 1:length(dl)) {
      ind <- is.na(x[, i])
      if (length(ind) > 0) 
        x[ind, i] <- dlordered[i] * 1.2
    }
    xOrig <- x
    if (!is.null(nPred) & length(nPred) == 1) {
      nPred <- rep(nPred, ncol(x))
    }
    if (!is.null(nPred) & length(nPred) > 1) {
      stop("nPred must be NULL or a vector of length 1.")
    }
    if (is.null(nPred) & variation) {
      ptmcv <- proc.time()
      if (verbose) 
        cat("\n cross validation to estimate number of predictors\n")
      ii <- 1
      if (verbose) 
        pb <- txtProgressBar(min = 0, max = sum(indNA), style = 3)
      nPred <- numeric(nrow(x))
      rtmspe <- NULL
      for (i in which(indNA)) {
        xneworder <- cbind(x[, i, drop = FALSE], x[, -i, 
                                                   drop = FALSE])
        rv <- variation(x, method = "Pivot")[1, ]
        cve <- numeric()
        for (np in c(3:ncol(x))) {
          s <- sort(rv)[np]
          cols <- which(rv <= s)[1:np]
          xn <- xneworder[, cols]
          xilr <- data.frame(pivotCoord(xn))
          colnames(xilr)[1] <- "Y"
          call <- call(method, formula = Y ~ .)
          cve[np] <- suppressWarnings(cvFit(call, data = xilr, 
                                            y = xilr$Y, cost = cvTools::rtmspe, K = 5, 
                                            R = 1, costArgs = list(trim = 0.1), seed = 1234)$cv)
        }
        nPred[i] <- which.min(cve)
        if (verbose) 
          setTxtProgressBar(pb, ii)
        ii <- ii + 1
      }
      if (verbose) 
        close(pb)
      ptmcv <- proc.time() - ptmcv
    }
    if (verbose) 
      cat("\n start the iteration:")
    it <- 1
    criteria <- 99999999
    while (it <= maxit & criteria >= eps) {
      if (verbose) 
        cat("\n iteration", it, "; criteria =", criteria)
      xold <- x
      for (i in which(indNA)) {
        if (verbose) 
          cat("\n replacement on (sorted) part", i)
        xneworder <- cbind(x[, i, drop = FALSE], x[, -i, 
                                                   drop = FALSE])
        if (variation) {
          orig <- xneworder
          rv <- variation(x, method = "Pivot")[1, ]
          s <- sort(rv)[nPred[i]]
          cols <- which(rv <= s)[1:nPred[i]]
          xneworder <- xneworder[, cols]
        }
        forphi <- cbind(rep(dlordered[i], n), xneworder[, 
                                                        -1, drop = FALSE])
        if (any(is.na(forphi))) 
          (break)()
        phi <- pivotCoord(forphi)[, 1]
        xneworder[xneworder < 2 * .Machine$double.eps] <- 2 * 
          .Machine$double.eps
        xilr <- data.frame(pivotCoord(xneworder))
        response <- as.matrix(xilr[, 1, drop = FALSE])
        predictors <- as.matrix(xilr[, -1, drop = FALSE])
        if (method == "lm") {
          reg1 <- lm(response ~ predictors) ##PF # ,subset = wn[,i])
          s <- sqrt(sum(reg1$residuals^2)/(sum(wn[, i]) - 
                                             ncol(predictors) - 1))
          yhat <- predict(reg1, newdata = data.frame(predictors))
        }
        else if (method == "MM") {
          reg1 <- MASS::rlm(response ~ predictors, method = "MM", 
                            maxit = 100 ) ## PF$ , subset = wn[, i])
          s <- reg1$s
          yhat <- predict(reg1, newdata = data.frame(predictors))
        }
        else if (method == "lmrob") {
          reg1 <- robustbase::lmrob(response ~ predictors, 
                                    max.it = 500, maxit.scale = 500) ## PF# , subset = wn[,i])
          s <- reg1$scale
          yhat <- predict(reg1, newdata = data.frame(predictors))
        }
        else if (method == "pls") {
          if (it == 1 & pre) {
            ## begin PF #
            #nC[i] <- bootnComp(xilr[, 2:ncol(xilr), drop = FALSE], 
            #  y = xilr[, 1], R, plotting = FALSE)$res
            nC[i] <- bootnComp(xilr[, 2:ncol(xilr), drop = FALSE], 
                               y = xilr[, 1], R, plotting = FALSE)$res3
            ## end PF #
          }
          if (verbose) 
            cat("   ;   ncomp:", nC[i])
          reg1 <- mvr(as.matrix(response) ~ as.matrix(predictors), 
                      ncomp = nC[i], method = "simpls") ## PF# , subset = wn[,i])
          yhat <- predict(reg1, newdata = data.frame(predictors), 
                          ncomp = nC[i])
          ##PF# s <- sqrt(sum(reg1$residuals^2)/nrow(xilr))
          s <- sqrt(sum(reg1$residuals^2)/length(reg1$residuals))
        }
        yhat <- as.numeric(yhat)
        # plot(response,yhat);abline(c(0,1))
        ex <- (phi - yhat)/s
        if (correction == "normal") {
          yhat2sel <- ifelse(dnorm(ex[w[, i]]) > 1e-05, 
                             yhat[w[, i]] + s * dnorm(ex[w[, i]])/(1 - pnorm(ex[w[,i]])), yhat[w[, i]])
        }
        else if (correction == "density") {
          stop("correction equals density is no longer be supported")
          # den <- density(ex[w[, i]])
          # ##PF# distr <- sROC::kCDF(ex[w, i])
          # distr <- sROC::kCDF(ex[w[, i]])
        }
        if (any(is.na(yhat)) || any(yhat == "Inf")) 
          stop("Problems in ilr because of infinite or NA estimates")
        ## begin PF #
        if (any(yhat2sel <= phi[w[, i]])) {
          yhat2sel <- ifelse(yhat2sel < phi[w[, i]], phi[w[,i]], yhat2sel)
        }
        ## end PF #
        xilr[w[, i], 1] <- yhat2sel
        xinv <- pivotCoordInv(xilr)
        if (variation == TRUE) {
          xneworder <- adjustImputed(xinv, xneworder, w2[, 
                                                         cols])
          orig[, cols] <- xneworder
          xinv <- orig
        }
        if (i %in% 2:(d - 1)) {
          xinv <- cbind(xinv[, 2:i], xinv[, c(1, (i + 1):d)])
        }
        if (i == d) {
          xinv <- cbind(xinv[, 2:d], xinv[, 1])
        }
        if (!variation) {
          x <- adjustImputed(xinv, xOrig, w2)
        }
        else {
          x <- xinv
        }
      }
      it <- it + 1
      criteria <- sum(((xold - x)/x)^2, na.rm = TRUE)
      if (verbose & criteria != 0) 
        cat("\n iteration", it, "; criteria =", criteria)
    }
    if (noise) {
      for (i in which(indNA)) {
        if (verbose) 
          cat("\n add noise on variable", i)
        inderr <- w[, i]
        if (noisemethod == "residuals") {
          error <- sample(residuals(reg1)[inderr], size = wcol[i], 
                          replace = TRUE)
          reg1$res[inderr] <- error
        }
        else {
          mu <- median(residuals(reg1)[inderr])
          sigma <- mad(residuals(reg1)[inderr])
          error <- rnorm(wcol[i], mean = mu, sd = sigma)
          reg1$res[inderr] <- error
        }
        yhat[inderr] <- yhat[inderr] + error
        s <- sqrt(sum(reg1$res^2)/nrow(xilr))
        ex <- (phi - yhat)/s
        yhat2sel <- ifelse(dnorm(ex[w[, i]]) > .Machine$double.eps, 
                           yhat[w[, i]] + s * dnorm(ex[w[, i]])/(1 - pnorm(ex[w[, 
                                                                                i]])), yhat[w[, i]])
        if (any(is.na(yhat)) || any(yhat == "Inf")) 
          stop("Problems in ilr because of infinite or NA estimates")
        xilr[w[, i], 1] <- yhat2sel
        xinv <- pivotCoordInv(xilr)
        if (i %in% 2:(d - 1)) {
          xinv <- cbind(xinv[, 2:i], xinv[, c(1, (i + 1):d)])
        }
        if (i == d) {
          xinv <- cbind(xinv[, 2:d], xinv[, 1])
        }
      }
    }
    x <- x[, order(o)]
    colnames(x) <- cn
    if (verbose) {
      message(paste(sum(w)), "values above detection limit have been imputed \n above the corresponding detection limits")
    }
    checkDL <- function(x, dl, indexNA) {
      check <- logical(ncol(x))
      for (i in 1:ncol(x)) {
        critvals <- x[indexNA[, i], i] < dl[i]
        check[i] <- any(critvals)
        if (check[i]) {
          ## PF begin #
          # x[which(x[indexNA[, i], i] < dl[i]), i] <- dl[i]
          ind <- x[indexNA[, i], i] < dl[i]
          x[indexNA[,i],i][ind] <- dl[i]
          ## PF end $
        }
      }
      if (any(check) & verbose) {
        message(paste(sum(critvals), "/", sum(w), "replaced values has been corrected"))
      }
      x
    }
    x <- checkDL(x, dl, indexFinalCheck)
    res <- list(x = x, criteria = criteria, iter = it, maxit = maxit, 
                wind = w, nComp = nC, nPred = nPred, variation = variation, 
                method = method, dl = dl)
    return(res)
  }

