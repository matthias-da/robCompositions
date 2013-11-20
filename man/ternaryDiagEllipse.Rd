\name{ternaryDiagEllipse}
\alias{ternaryDiagEllipse}
\title{
Adds tolerance ellipses to a ternary diagram.
}
\description{
Low-level plot function which add tolerance ellipses to a high-level plot of a ternary diagram.
}
\usage{
ternaryDiagEllipse(x, tolerance = c(0.9, 0.95, 0.975), locscatt = "MCD", ...)
}
\arguments{
  \item{x}{
       Three-part composition. Object of class \dQuote{matrix} or \dQuote{data.frame}.
}
  \item{tolerance}{
Determines the amount of observations with Mahalanobis distance larger than the drawn ellipse, scaled to one.
}
  \item{locscatt}{
Method for estimating the mean and covariance.
}
  \item{\dots}{
Additional arguments passed trough.
}
}
\value{
no values are returned.
}
\author{
Peter Filzmoser, Matthias Templ
}
\seealso{
\code{\link{ternaryDiag}}
}
\examples{
data(coffee)
x <- coffee[,1:3]
ternaryDiag(x, grid=FALSE)
ternaryDiagEllipse(x)
## or directly:
ternaryDiag(x, grid=FALSE, line="ellipse")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }