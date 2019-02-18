#' Geometric mean
#' 
#' Computes the geometric mean(s) of a numeric vector, matrix or data.frame
#' 
#' @rdname gmean
#' @aliases gmean_sum 
#' @param x matrix or data.frame with numeric entries
#' @param margin a vector giving the subscripts which the function will be applied over, 
#' 1 indicates rows, 2 indicates columns, 3 indicates all values.
#' @author Matthias Templ
#' @details \code{gmean_sum} calculates the totals based on geometric means while \code{gmean}
#' calculates geometric means on rows (margin = 1), on columns (margin = 2), or on all values (margin = 3)
#' @return geometric means (if \code{gmean} is used) or totals (if \code{gmean_sum} is used)
#' @export
#' @examples 
#' data("precipitation")
#' gmean_sum(precipitation)
#' gmean_sum(precipitation, margin = 2)
#' gmean_sum(precipitation, margin = 1)
#' gmean_sum(precipitation, margin = 3)
#' addmargins(precipitation)
#' addmargins(precipitation, FUN = gmean_sum)
#' addmargins(precipitation, FUN = mean)
#' addmargins(precipitation, FUN = gmean)
#' 
#' data("arcticLake", package = "robCompositions")
#' gmean(arcticLake$sand)
#' gmean(as.numeric(arcticLake[1, ]))
#' gmean(arcticLake)
#' gmean(arcticLake, margin = 1)
#' gmean(arcticLake, margin = 2)
#' gmean(arcticLake, margin = 3)
gmean_sum <-
  function (x, margin = NULL) {
    if(any(is.na(x))) warning("missing values in x")
    if(is.vector(x)){
      if(!is.numeric(x)) stop("x is not of class numeric")
      ind <- is.finite(x) & x > 0
      res <- exp(mean(log(unclass(x)[ind]))) * length(ind)
    } else if (is.matrix(x) | is.data.frame(x) | is.table(x) | is.array(x)){
      if(is.null(margin)) margin = 2
      if(margin %in% 1:2){
        res <- as.numeric(apply(x, margin, function(x){
          ind <- is.finite(x) & x > 0
          exp(mean(log(unclass(x)[ind]))) * length(ind)
        }))
      } else if(margin == 3){
        x <- unclass(c(x))
        ind <- is.finite(x) & x > 0
        res <- exp(mean(log(x[ind]))) * length(ind)
      }
    } else {
      stop("x is not of class numeric, matrix or data.frame")
    }
    return(res)
  }

#' @aliases gmean_sum
#' @rdname gmean
#' @export
gmean <-
  function (x, margin = NULL) {
    if(any(is.na(x))) warning("missing values in x")
    if(!is.null(margin)){
      if(margin == 3){x <- as.numeric(unlist(c(x)))}
    }
    if(is.vector(x)){
      if(!is.numeric(x)) stop("x is not of class numeric")
      ind <- is.finite(x) & x > 0
      res <- exp(mean(log(unclass(x)[ind]))) 
    } else if (is.matrix(x) | is.data.frame(x)){
      if(is.null(margin)) margin = 2
      res <- as.numeric(apply(x, margin, function(x){
        ind <- is.finite(x) & x > 0
        exp(mean(log(unclass(x)[ind]))) 
      }))
    } else {
      stop("x is not of class numeric, matrix or data.frame")
    }
    return(res)
}
