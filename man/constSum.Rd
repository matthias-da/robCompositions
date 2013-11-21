\name{constSum}
\alias{constSum}
\title{
Constant sum
}
\description{
Closes compositions to sum up to a given constant (default 1), by
     dividing each part of a composition by its row sum. 
}
\usage{
constSum(x, const=1, na.rm=TRUE)
}
\arguments{
  \item{x}{
multivariate data ideally of class data.frame or matrix
}
  \item{const}{
constant, the default equals 1.
}
  \item{na.rm}{
removing missing values.
}
}
\value{
The data for which the row sums are equal to \code{const}.
}
\author{
Matthias Templ
}
\seealso{
\code{\link[compositions]{clo}}
}
\examples{
data(expenditures)
constSum(expenditures)
constSum(expenditures, 100)
}
\keyword{ manip }