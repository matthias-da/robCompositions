#' Perturbation and powering
#' 
#' Perturbation and powering for two compositions.
#' 
#' @aliases perturbation powering
#' @param x (compositional) vector containing positive values
#' @param y (compositional) vector containing positive values or NULL for powering
#' @return Result of perturbation or powering
#' @author Matthias Templ
#' @export
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman and Hall Ltd., London (UK). 416p.
#' @keywords math arith
#' @examples
#' data(expenditures)
#' x <- expenditures[1 ,]
#' y <- expenditures[2, ]
#' perturbation(x, y)
#' powering(x, 2)
perturbation <- function(x, y){
  constSum(x * y)
}

#' @rdname perturbation
#' @param a constant, numeric vector of length 1
#' @export
powering <- function(x, a){
  constSum(x^a)
}
