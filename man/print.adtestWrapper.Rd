\name{print.adtestWrapper}
\alias{print.adtestWrapper}
\title{ print method for objects of class adtestWrapper }
\description{
Provides a short print output as shown in the examples.
}
\usage{
\method{print}{adtestWrapper}(x, ...)
}
\arguments{
  \item{x}{  object of class \sQuote{adtestWrapper} }
  \item{\dots}{ \code{\dots} }
}
\details{
  Have a look at the example, it's self-explaining.
}
\value{
nothing
}
\author{ Matthias Templ and Karel Hron }
\seealso{ \code{\link{adtestWrapper}}, \code{\link{summary.adtestWrapper}} }
\examples{
data(machineOperators)
a <- adtestWrapper(machineOperators, R=50) # choose higher value of R
a
summary(a)
}
\keyword{ print }
