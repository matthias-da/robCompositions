#' Classical estimates for tables
#' 
#' Some standard/classical (non-compositional) statistics 
#' 
#' @param x a data.frame, matrix or table
#' @param margins margins
#' @param statistics statistics of interest
#' @param maggr a function for calculating the mean margins of a table, default is the arithmetic mean
#' @details statistics \sQuote{phi} is the values of the table divided by the product of margins. \sQuote{cramer} normalize these values according to the dimension of the table. \sQuote{chisq} are the expected values according to Pearson while \sQuote{yates} according to Yates.
#' 
#' For the \code{maggr} function argument, arithmetic means (\code{mean}) should be chosen to obtain the classical results. Any other user-provided functions should be take with care since the classical estimations relies on the arithmetic mean.
#' @author Matthias Templ
#' @return List containing all statistics
#' @references 
#' Egozcue, J.J., Pawlowsky-Glahn, V., Templ, M., Hron, K. (2015)
#' Independence in contingency tables using simplicial geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, 44 (18), 3978--3996.
#' 
#' @export
#' @examples 
#' data(precipitation) 
#' tab1 <- indTab(precipitation)
#' stats(precipitation)
#' stats(precipitation, statistics = "cramer")
#' stats(precipitation, statistics = "chisq")
#' stats(precipitation, statistics = "yates")
#' 
#' ## take with care 
#' ## (the provided statistics are not designed for that case):
#' stats(precipitation, statistics = "chisq", maggr = gmean)
stats <- function(x, margins=NULL, 
            statistics = c("phi", "cramer", "chisq", "yates"), maggr = mean){
  ## x ... prop.table
  if (!is.matrix(x))
    stop("Function only defined for 2-way tables.")
  if( is.null( margins ) ){
    m1 <- apply(x, 1, maggr) #function(x) get(sum.stat)(x))
    m2 <- apply(x, 2, maggr) #function(x) get(sum.stat)(x))
  } else {
    if(is.list(margins)){
      m1 <- margins[[1]]
      m2 <- margins[[2]]
    }
    if(is.matrix(margins) || is.data.frame(margins)){
      m1 <- margins[,1]
      m2 <- margins[,2]			
    }
    if(!is.null(margins) || !is.list(margins) || !is.matrix(margins) || !is.data.frame(margins)){
      stop(paste("class", class(margins)[1], "of margins is not supported"))
    }
    if((length(m1) != nrow(x) || length(m2) != ncol(x))) stop("wrong length of margins")
  }
  method <- match.arg(statistics)
  stat <- function(x, method, m1, m2) {
    evals <- m1 %*% t(m2) 
    phi <- x / evals
    switch(method,
        phi = x / m1 %*% t(m2),
        cramer = sqrt(phi^2 / min(dim(x) - 1)),
        chisq = sqrt((x - evals)^2/evals),
        yates = sqrt( (abs(x - evals) - 0.5)^2 / evals ) 
    )
  }
  return(stat(x, method, m1, m2))
#  evals <- m1 %*% t(m2)
#  phi <- x / evals
#  cramer <- sqrt(phi^2 / min(dim(x) - 1))
#  chisq <- sqrt((x - evals)^2/evals)
#  yates <- sqrt( (abs(x - evals) - 0.5)^2 / evals ) 
#  list(phi=phi, cramer=cramer, chisq=chisq, yates=yates)	
} 
