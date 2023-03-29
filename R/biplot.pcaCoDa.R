#' Biplot method
#' 
#' Provides robust compositional biplots.
#' 
#' The robust compositional biplot according to Aitchison and Greenacre (2002),
#' computed from (robust) loadings and scores resulting from \code{\link{pcaCoDa}}, is performed.
#' 
#' @param x object of class \sQuote{pcaCoDa}
#' @param y ...
#' @param \dots arguments passed to plot methods
#' @param choices selection of two principal components by number. Default: c(1,2)
#' @return The robust compositional biplot.
#' @author M. Templ, K. Hron
#' @seealso \code{\link{pcaCoDa}}, \code{\link{plot.pcaCoDa}}
#' @references Aitchison, J. and Greenacre, M. (2002). Biplots of compositional
#' data. \emph{Applied Statistics}, \bold{51}, 375-392. \
#' 
#' Filzmoser, P., Hron, K., Reimann, C. (2009) Principal component analysis for
#' compositional data with outliers. \emph{Environmetrics}, \bold{20} (6),
#' 621--632.
#' @keywords aplot
#' @export
#' @method biplot pcaCoDa
#' @examples
#' 
#' data(coffee)
#' p1 <- pcaCoDa(coffee[,-1])
#' p1
#' plot(p1, which = 2, choices = 1:2)
#' 
#' # exemplarly, showing the first and third PC
#' a <- p1$princompOutputClr
#' biplot(a, choices = c(1,3))
#' 
#' 
#' ## with labels for the scores:
#' data(arcticLake)
#' rownames(arcticLake) <- paste(sample(letters[1:26], nrow(arcticLake), replace=TRUE), 
#'                               1:nrow(arcticLake), sep="")
#' pc <- pcaCoDa(arcticLake, method="classical")
#' plot(pc, xlabs=rownames(arcticLake), which = 2)
#' plot(pc, xlabs=rownames(arcticLake), which = 3)
#' 

biplot.pcaCoDa <- function(x, y, ..., choices = 1:2){
  ## biplot
  #z <- list()
  #z$scores <- x$scores
  x$princompOutputClrz$loadings <- x$loadings
  if(!exists("choices")){
    choices <- 1:2
  }
    beschx <- if(x$method == "robust") paste0("PC", choices[1], "(clr-robust)") else "PC1 (clr-classical)"
    beschy <- if(x$method == "robust") paste0("PC", choices[2], "(clr-robust)") else "PC2 (clr-classical)"
  biplot(x$princompOutputClr, xlab=beschx, ylab=beschy, ...)
}
