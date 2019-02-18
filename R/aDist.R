#' Aitchison distance
#' 
#' Computes the Aitchison distance between two observations, between two data
#' sets or within observations of one data set.
#' 
#' This distance measure accounts for the relative scale property of
#' compositional data. It measures the distance between two compositions if
#' \code{x} and \code{y} are vectors. It evaluates the sum of the distances between
#' \code{x} and \code{y} for each row of \code{x} and \code{y} if \code{x} and
#' \code{y} are matrices or data frames. It computes a n times n distance matrix (with n
#' the number of observations/compositions) if only \code{x} is provided.
#' 
#' 
#' The underlying code is partly written in C and allows a fast computation also for
#' large data sets whenever \code{y} is supplied.
#' 
#' @aliases aDist iprod
#' @param x a vector, matrix or data.frame
#' @param y a vector, matrix or data.frame with equal dimension as \code{x} or NULL.
#' @return The Aitchison distance between two compositions or between two data
#' sets, or a distance matrix in case code{y} is not supplied.
#' @author Matthias Templ, Bernhard Meindl
#' @export
#' @useDynLib robCompositions
#' @seealso \code{\link{pivotCoord}}
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman and Hall Ltd., London (UK). 416p.
#' 
#' Aitchison, J. and Barcelo-Vidal, C. and Martin-Fernandez, J.A. and
#' Pawlowsky-Glahn, V. (2000) Logratio analysis and compositional distance.
#' \emph{Mathematical Geology}, \bold{32}, 271-275.
#' 
#' Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values
#' for compositional data using classical and robust methods
#' \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages
#' 3095-3107.
#' @keywords math arith
#' @examples
#' 
#' data(expenditures)
#' x <- xOrig <- expenditures
#' ## Aitchison distance between two 2 observations:
#' aDist(x[1, ], x[2, ])
#' 
#' ## Aitchison distance of x:
#' aDist(x)
#' 
#' ## Example of distances between matrices:
#' ## set some missing values:
#' x[1,3] <- x[3,5] <- x[2,4] <- x[5,3] <- x[8,3] <- NA
#' 
#' ## impute the missing values:
#' xImp <- impCoda(x, method="ltsReg")$xImp
#' 
#' ## calculate the relative Aitchsion distance between xOrig and xImp:
#' aDist(xOrig, xImp)
#' 
`aDist` <-
  function(x, y = NULL){
    if(!is.null(y)){
      if(is.vector(x)) x <- matrix(x, ncol=length(x))
  	  if(is.vector(y)) y <- matrix(y, ncol=length(y))	  
  	  n <- dim(x)[1]
  	  p <- D <- dim(x)[2]
  	  rn <- rownames(x)
  	  matOrig <- as.numeric(t(x))
  	  matImp <- as.numeric(t(y))
  	  dims <- as.integer(c(n, p))
  	  rowDists <-  as.numeric(rep(0.0, n))
  	  distance <- as.numeric(0.0)
  	  out <- .C("da", 
  				  matOrig,
  				  matImp,
  				  dims,
  				  rowDists,
  				  distance,
  				  PACKAGE="robCompositions", NUOK=TRUE
  		  )[[5]]
  #     } else if(is.null(y) & method == "R"){
  #       out <- matrix(, ncol = n, nrow = n)
  #       gms <- apply(x, 1, function(x) gm(as.numeric(x)))
  #       for(i in 1:(n-1)){
  #         for(j in (i+1):n){
  #           out[i, j] <- out[j, i] <- 
  #             sqrt(sum((log(as.numeric(x[i, ]) / gms[i]) - 
  #                        log(as.numeric(x[j, ]) / gms[j]))^2))
  #         }
  #       }
  #       diag(out) <- 0
  #       rownames(out) <- colnames(out) <- rn
    } else {
      if(is.vector(x)) x <- matrix(x, ncol=length(x))
      n <- dim(x)[1]
      p <- D <- dim(x)[2]
      rn <- rownames(x)
      out <- dist(cenLR(x)$x.clr)
    }
	  return(out)
}	  

#' @rdname aDist
#' @export
#' @examples 
#' data("expenditures") 
#' aDist(expenditures)  
#' x <- expenditures[, 1]
#' y <- expenditures[, 2]
#' aDist(x, y)
#' aDist(expenditures, expenditures)
iprod <- function(x, y){
  warning("wrong formula, has to be fixed.")
  D <- length(x)
  if(D != length(y)) stop("x and y should have the same length")
  ip <- 1 / D * sum(log(as.numeric(x[1:(D-1)]) / as.numeric(x[2:D])) * 
                      log(as.numeric(y[1:(D-1)]) / as.numeric(y[2:D])))
  return(ip)
}
	
