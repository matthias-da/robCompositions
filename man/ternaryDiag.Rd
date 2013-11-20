\name{ternaryDiag}
\alias{ternaryDiag}
\title{ Ternary diagram}
\description{
  This plot shows the relative proportions of three variables (compositional parts) in one diagramm. 
  Before plotting, the data are scaled.
}
\usage{
ternaryDiag(x, name = colnames(x), grid = TRUE, gridCol=grey(0.6), mcex = 1.2, line = "none", robust = TRUE, group = NULL, tol = 0.975, ...)
}
\arguments{
  \item{x}{ matrix or data.frame with 3 columns }
  \item{name}{ names of the variables }
  \item{grid}{ if TRUE a grid is plotted additionally in the ternary diagram }
  \item{gridCol}{ color for the grid lines }
  \item{mcex}{ label size }
  \item{line}{ may be set to  \dQuote{none}, \dQuote{pca}, \dQuote{regression}, \dQuote{regressionconf}, \dQuote{regressionpred}, \dQuote{ellipse}, \dQuote{lda} }
  \item{robust}{ if line equals TRUE, it dedicates if a robust estimation is applied or not. }
  \item{group}{ if line equals \dQuote{da}, it determines the grouping variable }
  \item{tol}{ if line equals \dQuote{ellipse}, it determines the parameter for the tolerance ellipse }
  \item{\dots}{ further parameters, see, e.g., \code{par()} }
}
\details{
The relative proportions of each variable are plotted.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. 
}
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}, Matthias Templ
}
\seealso{
\code{\link[StatDA]{ternary}}
}
\examples{
data(arcticLake)
ternaryDiag(arcticLake)

data(coffee)
x <- coffee[,1:3]
grp <- as.integer(factor(coffee[,4]))
ternaryDiag(x, col=grp, pch=grp)
ternaryDiag(x, grid=FALSE, col=grp, pch=grp)
legend("topright", legend=unique(coffee[,4]), pch=1:2, col=1:2)

ternaryDiag(x, grid=FALSE, col=grp, pch=grp, line="ellipse", tol=c(0.975,0.9), lty=2)
ternaryDiag(x, grid=FALSE, line="pca")
ternaryDiag(x, grid=FALSE, col=grp, pch=grp, line="pca", lty=2, lwd=2)
}
\keyword{ multivariate }
\keyword{ aplot }