\name{summary.imp}
\alias{summary.imp}
\title{ Summary method for objects of class imp }
\description{
  A short comparison of the original data and the imputed data is given.
}
\usage{
\method{summary}{imp}(object, ...)
}
\arguments{
  \item{object}{ an object of class \sQuote{imp} }
  \item{\dots}{ additional arguments passed trough }
}
\details{
  Note that this function will be enhanced with more sophisticated methods in 
  future versions of the package.
  It is very rudimental in its present form. 
}
\value{
None (invisible NULL).
}
\author{ Matthias Templ }
\seealso{ \code{\link{impCoda}}, \code{\link{impKNNa}} }
\examples{
data(expenditures)
expenditures[1,3]
expenditures[1,3] <- NA
xi <- impKNNa(expenditures)
xi
summary(xi)
# plot(xi, which=1:2)
}
\keyword{ print }
