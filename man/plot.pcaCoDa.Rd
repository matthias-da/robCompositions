\name{plot.pcaCoDa}
\alias{plot.pcaCoDa}
\title{ Plot method }
\description{
Provides robust compositional biplots.
}
\usage{
\method{plot}{pcaCoDa}(x, y, ...)
}
\arguments{
  \item{x}{ object of class \sQuote{pcaCoDa} }
  \item{y}{ ... }
  \item{\dots}{ ... }
}
\details{
The robust compositional biplot according to Aitchison and Greenacre (2002), computed from resulting (robust) loadings and scores, 
is performed.
}
\value{
The robust compositional biplot.
}
\references{ 
 Aitchison, J. and Greenacre, M. (2002). Biplots of compositional data. \emph{Applied Statistics}, \bold{51}, 375-392. \\

 Filzmoser, P., Hron, K., Reimann, C. (2009)
 Principal Component Analysis for Compositional Data with Outliers. \emph{Environmetrics}, \bold{20} (6), 621--632.
 }
\author{ M. Templ, K. Hron }
\seealso{ \code{\link{pcaCoDa}}}
\examples{
data(expenditures)
p1 <- pcaCoDa(expenditures)
p1
plot(p1)


## with labels for the scores:
data(arcticLake)
rownames(arcticLake) <- paste(sample(letters[1:26],nrow(arcticLake), replace=TRUE), 
                              1:nrow(arcticLake), sep="")
pc <- pcaCoDa(arcticLake, method="standard")
plot(pc, xlabs=rownames(arcticLake))

}
\keyword{ aplot }
