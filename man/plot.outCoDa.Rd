\name{plot.outCoDa}
\alias{plot.outCoDa}
\title{ plot method for outCoDa objects }
\description{
Plots the Mahalanobis distance.
}
\usage{
\method{plot}{outCoDa}(x, y, ...)
}
\arguments{
  \item{x}{ object from class \sQuote{outCoDa} }
  \item{y}{ ... }
  \item{\dots}{ ... }
}
\details{
The dashed line indicates the (1 - alpha) quantile of the Chi-squared distribution. Observations with Mahalanobis distance greater than this quantile could be considered as  
compositional outliers.
}
\references{ Filzmoser, P., and Hron, K. (2008)
  Outlier detection for compositional data using robust methods. \emph{Math. Geosciences}, \bold{40} 233-248.
}
\author{ Matthias Templ }
\seealso{ \code{\link{outCoDa}} }
\examples{
data(expenditures)
oD <- outCoDa(expenditures)
oD
plot(oD)
}
\keyword{ hplot }
