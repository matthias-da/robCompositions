\name{summary.adtestWrapper}
\alias{summary.adtestWrapper}
\title{ summary method for objects of class adtestWrapper }
\description{
Provides a summary as shown in the examples.
}
\usage{
\method{summary}{adtestWrapper}(object, ...)
}
\arguments{
  \item{object}{ object of class \sQuote{adtestWrapper} }
  \item{\dots}{ additional arguments passed through }
}
\details{
A similar output is proposed by (Pawlowsky-Glahn, et al. (2008). In addition to that,
p-values are provided.
}
\value{
a data frame including an information about the ilr-variables used (first column), the 
underlying test (second column), the test statistics (third column), the corresponding
estimated p-values (fourth column) and an information about the rejection of the null 
hypothesis (last column).
}
\references{ 
Pawlowsky-Glahn, V., Egozcue, J.J. and Tolosana-Delgado, R. (2008),
\emph{Lecture Notes on Compositional Data Analysis} Universitat de Girona,
\url{http://dugi-doc.udg.edu/bitstream/10256/297/1/CoDa-book.pdf}
}
\author{ Matthias Templ and Karel Hron }
\seealso{ \code{\link{adtestWrapper}} }
\examples{
data(machineOperators)
a <- adtestWrapper(machineOperators, R=50) # choose higher value of R
a
summary(a)
}
\keyword{ print }