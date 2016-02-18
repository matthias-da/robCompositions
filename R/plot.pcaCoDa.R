#' Plot method
#' 
#' Provides a screeplot for (robust) compositional principal components analysis.
#' 
#' 
#' @param x object of class \sQuote{pcaCoDa}
#' @param y ...
#' @param \dots ...
#' @return The robust compositional screeplot.
#' @author M. Templ, K. Hron
#' @seealso \code{\link{pcaCoDa}}, \code{\link{biplot.pcaCoDa}}
#' @references Filzmoser, P., Hron, K., Reimann, C. (2009) Principal Component Analysis for
#' Compositional Data with Outliers. \emph{Environmetrics}, \bold{20} (6),
#' 621--632.
#' @keywords aplot
#' @export
#' @method plot pcaCoDa
#' @examples
#' 
#' data(coffee)
#' p1 <- pcaCoDa(coffee[,-1])
#' plot(p1)
#' plot(p1, type="lines")
#' 
#' 
plot.pcaCoDa <- function(x, y, ...){
	beschx <- if(x$method == "robust") "PC1 (clr-robust)" else "PC1 (clr-standard)"
	beschy <- if(x$method == "robust") "PC2 (clr-robust)" else "PC2 (clr-standard)"
	screeplot(x$princompOutputClr, main="", ...)
}
