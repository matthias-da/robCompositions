\name{orthbasis}
\alias{orthbasis}
\title{
Orthonormal basis
}
\description{
Orthonormal basis from cenLR transformed data to isomLR transformated data.
}
\usage{
orthbasis(D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
number of parts (variables)
}
}
\details{
For the chosen balances for \dQuote{isomLR}, this is the orthonormal basis that transfers
the data from centered logratio to isometric logratio.
}
\value{
the orthonormal basis.
}
\author{
Karel Hron, Matthias Templ
}
\seealso{
\code{\link{isomLR}}, \code{\link{cenLR}}
}
\examples{
data(expenditures)
V <- orthbasis(ncol(expenditures))
xcen <- cenLR(expenditures)$x.clr
xi <- as.matrix(xcen) %*% V
xi2 <- isomLR(expenditures)
all.equal(xi, xi2)
}
\keyword{ manip }