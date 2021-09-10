#' Plot method
#' 
#' Provides a screeplot and biplot for (robust) compositional principal components analysis.
#' 
#' 
#' @param x object of class \sQuote{pcaCoDa}
#' @param y ...
#' @param \dots ...
#' @param which an integer between 1 and 3. Produces a screeplot (1), or a biplot using stats biplot.prcomp function (2), or a biplot using ggfortify's autoplot function (3).
#' @param choices principal components to plot by number
#' @return The robust compositional screeplot.
#' @author M. Templ, K. Hron
#' @seealso \code{\link{pcaCoDa}}, \code{\link{biplot.pcaCoDa}}
#' @references Filzmoser, P., Hron, K., Reimann, C. (2009) Principal Component Analysis for
#' Compositional Data with Outliers. \emph{Environmetrics}, \bold{20} (6),
#' 621--632.
#' @keywords aplot
#' @export
#' @import ggfortify 
#' @method plot pcaCoDa
#' @examples
#' 
#' data(coffee)
#' \dontrun{
#' p1 <- pcaCoDa(coffee[,-1])
#' plot(p1)
#' plot(p1, type="lines")
#' plot(p1, which = 2)
#' plot(p1, which = 3)
#' }
#' 
#' 
plot.pcaCoDa <- function(x, y, ..., which = 1, choices = 1:2){
  if(which == 1){
	screeplot(x$princompOutputClr, main="", ...)
  } else if (which == 2) {
    beschx <- if(x$method == "robust") "PC 1 (clr-robust)" else "PC 1 (clr-standard)"
    beschy <- if(x$method == "robust") "PC 2 (clr-robust)" else "PC 2 (clr-standard)"
    biplot(x, choices = choices)
  } else {
    dat <-  x$princompOutputClr
    dat$scale <- c(dat$scale, 1)
    dat$center <- c(dat$center, 0)
    autoplot(dat, loadings=TRUE, loadings.label = TRUE, loadings.label.repel=TRUE, label=TRUE, label.repel=TRUE, x = choices[1], y = choices[2])
  }
}
