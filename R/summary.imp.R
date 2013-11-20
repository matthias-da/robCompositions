summary.imp <- function(object, ...){
  geometricmean <- function (x) {
    if (any(na.omit(x == 0)))
        0
    else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
  }
gm <- apply(object$xOrig, 2, function(x) {
  geometricmean(as.numeric(x[complete.cases(x)]))
})
gmI <- apply(object$xImp, 2, function(x) {
  geometricmean(as.numeric(x[complete.cases(x)])) ## gewichten!
})

  d <- data.frame(orig=gm,
                  imp=gmI)
  cat("\n geometric mean of the original data and the imputed data: \n")

  ## Einfluss der Imputation mittels bootstrap schaetzen
  ## laut script sim

  d
}