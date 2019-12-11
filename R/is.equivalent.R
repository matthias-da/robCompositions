#' equivalence class
#' 
#' Checks if two vectors or two data frames are from the same equivalence class
#' 
#' @param x either a numeric vector, or a data.frame containing such vectors. 
#' @param y either a numeric vector, or a data.frame containing such vectors. 
#' @param tollerance numeric >= 0. Differences smaller than tolerance are not considered.
#' @return logical TRUE if the two vectors are from the same equivalence class.
#' @seealso \code{\link{all.equal}}
#' @author Matthias Templ
#' @export
#' @references Filzmoser, P., Hron, K., Templ, M. (2018) \emph{Applied Compositional Data Analysis}.
#' Springer, Cham.
#' @keywords manip
#' @examples
#' 
#' is.equivalent(1:10, 1:10*2)
#' is.equivalent(1:10, 1:10+1)
#' data(expenditures)
#' x <- expenditures
#' is.equivalent(x, constSum(x))
#' y <- x
#' y[1,1] <- x[1,1]+1
#' is.equivalent(y, constSum(x))
#' 
is.equivalent <- function(x, y, tollerance = .Machine$double.eps ^ 0.5){
  clInfo <- class(x)[1]
	if(clInfo != "integer" & clInfo != "numeric" & clInfo != "data.frame"){
	  stop("object x must be from class numeric or data.frame")
	}
  # x is numeric, y is numeric
  if((clInfo == "numeric" | clInfo == "integer") & (class(y) == "numeric" | class(y) == "integer")){
    x <- as.numeric(x)
    y <- as.numeric(y)
    fac <- x[1] / y[1]
    # test <- identical(x, y*fac)
    test <- all.equal(x, y*fac, tollerance = tollerance)
    if(!is.logical(test)) test <- FALSE
  }
  # x is a data.frame, y is a data.frame
  if((any(clInfo == "data.frame")) & (any(clInfo == "data.frame"))){
    n <- nrow(x)
    test <- logical(n)
    for(i in 1:n){
      suppressWarnings(test[i] <- all.equal(x[i,], y[i,] * x[i,1] / y[i,1], tollerance = tollerance))
    }
    if(!is.logical(test)){
      test <- FALSE
    }
    if(is.logical(test) & any(!test)){
      test <- FALSE
    }
    if(is.logical(test) & all(test)){
      test <- TRUE
    }
  }
	return(test)
}
