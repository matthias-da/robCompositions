#' Normalize a vector to length 1
#' 
#' Scales a vector to a unit vector.
#' 
#' @param x a numeric vector
#' @author Matthias Templ
#' @keywords manip
#' @export
#' @examples
#' 
#' data(expenditures)
#' i <- 1
#' D <- 6
#' vec <- c(rep(-1/i, i), 1, rep(0, (D-i-1)))
#' 
#' norm1(vec)
#' 
norm1 <- function(x) {
  stopifnot(is.numeric(x) | is.integer(x))
  x / sqrt(sum(x^2))
}
