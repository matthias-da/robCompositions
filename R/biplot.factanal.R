#' Biplot method
#' 
#' Provides robust compositional biplots.
#' 
#' The robust compositional biplot according to Aitchison and Greenacre (2002),
#' computed from resulting (robust) loadings and scores, is performed.
#' 
#' @param x object of class \sQuote{factanal}
#' @param \dots ...
#' @return The robust compositional biplot.
#' @author M. Templ, K. Hron
#' @seealso \code{\link{pfa}}
#' @references Aitchison, J. and Greenacre, M. (2002). Biplots of compositional
#' data. \emph{Applied Statistics}, \bold{51}, 375-392. \
#' 
#' Filzmoser, P., Hron, K., Reimann, C. (2009) Principal component analysis for
#' compositional data with outliers. \emph{Environmetrics}, \bold{20} (6),
#' 621--632.
#' @keywords aplot
#' @export
#' @method biplot factanal
#' @examples
#' data(expenditures)
#' res.rob <- pfa(expenditures, factors=2, scores = "regression")
#' biplot(res.rob)
biplot.factanal <- function(x, ...){
  if(is.null(x$scores)) stop("no scores computed")
  if(x$factors == 1) stop("only one factor avalable, but need two")
  beschx <- if(x$robust) "Factor1 (clr-robust)" else "Factor1 (clr-classical)"
  beschy <- if(x$robust) "Factor2 (clr-robust)" else "Factor2 (clr-classical)"
  biplot(x$scores, x$loadings, xlab=beschx, ylab=beschy, ...)
}
