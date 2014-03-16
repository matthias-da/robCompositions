\name{gm}
\alias{gm}
\title{
geometric mean
}
\description{
This function calculates the geometric mean.
}
\usage{
gm(x)
}
\arguments{
  \item{x}{
    A numeric vector.
}
}
\details{
Calculates the geometric mean of all positive entries of a vector.
}
\value{
The geometric mean.
}
\author{
Matthias Templ
}
\examples{
gm(runif(100))
}
\keyword{math}
