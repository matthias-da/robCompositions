\name{ternaryDiagPoints}
\alias{ternaryDiagPoints}
\alias{ternaryDiagLines}
\title{
Add points or lines to a given ternary diagram.
}
\description{
Low-level plot function to add points or lines to a ternary high-level plot.
}
\usage{
ternaryDiagPoints(x, ...)
ternaryDiagLines(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Three-dimensional composition given as an object of class \dQuote{matrix} or \dQuote{data.frame}.}
  \item{\dots}{Additional graphical parameters passed through.}
}
\value{
no values are returned.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. 
}
\author{
Matthias Templ
}
\seealso{
\code{\link{ternaryDiag}}
}
\examples{
data(coffee)
x <- coffee[,1:3]
ternaryDiag(x, grid=FALSE)
ternaryDiagPoints(x+1, col="red", pch=2)
}
\keyword{ aplot }