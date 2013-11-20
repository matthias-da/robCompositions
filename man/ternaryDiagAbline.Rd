\name{ternaryDiagAbline}
\alias{ternaryDiagAbline}
\title{
Adds a line to a ternary diagram.
}
\description{
A low-level plot function which adds a line to a high-level ternary diagram.
}
\usage{
ternaryDiagAbline(x, ...)
}
\arguments{
  \item{x}{
Two-dimensional data set in isometric log-ratio transformed space.
}
  \item{\dots}{
Additional graphical parameters passed through.
}
}
\details{
This is a small utility function which helps to add a line in a ternary plot from two given points in an isometric transformed space.
}
\value{
no values are returned.
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
ternaryDiagAbline(data.frame(z1=c(0.01,0.5), z2=c(0.4,0.8)), col="red")
}
\keyword{ aplot }