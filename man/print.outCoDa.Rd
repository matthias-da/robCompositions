\name{print.outCoDa}
\alias{print.outCoDa}
\title{ print method for outCoDa objects }
\description{
Gives a short information of the amount of outliers in objects of class \sQuote{outCoDa}.
}
\usage{
\method{print}{outCoDa}(x, ...)
}
\arguments{
  \item{x}{object of class \sQuote{ourCoDa} }
  \item{\dots}{ ... }
}
\author{ Matthias Templ, Karel Hron }
\seealso{ \code{\link{outCoDa}} }
\examples{
data(expenditures)
oD <- outCoDa(expenditures)
oD
}
\keyword{ print }
