\name{print.imp}
\alias{print.imp}
\title{ Print method for objects of class imp }
\description{
 The function returns a few information about how many missing values are imputed and possible other 
 information about the amount of iterations, for example.
}
\usage{
\method{print}{imp}(x, ...)
}
\arguments{
  \item{x}{ an object of class \sQuote{imp} }
  \item{\dots}{ additional arguments passed trough }
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
xi <- impCoda(expenditures)
xi
summary(xi)
plot(xi, which=1:2)
}
\keyword{ print }
