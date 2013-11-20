\name{print.pcaCoDa}
\alias{print.pcaCoDa}
\title{ Print method for pcaCoDa objects}
\description{
Print method for objects of class \sQuote{pcaCoDa}. 
}
\usage{
\method{print}{pcaCoDa}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \sQuote{pcaCoDa} }
  \item{\dots}{ ... }
}
\value{
Prints the (cummulative) percentages of explained variability for clr transformed data by principal component analysis.
}
\author{ M. Templ, K. Hron }
\seealso{ \code{\link{pcaCoDa}}, \code{\link{plot.pcaCoDa}}}
\examples{
data(expenditures)
p1 <- pcaCoDa(expenditures)
p1
plot(p1)
}
\keyword{ print }
