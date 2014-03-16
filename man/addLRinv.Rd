\name{addLRinv}
\alias{addLRinv}
\title{
Additive logistic transformaton
}
\description{
Inverse additive log-ratio transformation, often called additive logistic transformation.
}
\usage{
addLRinv(x, cnames = NULL, ivar = NULL, useClassInfo = TRUE)
}
\arguments{
  \item{x}{
data set, object of class \dQuote{alr}, \dQuote{matrix} or \dQuote{data.frame}
}
  \item{cnames}{
column names. If the object is of class \dQuote{alr} the column names are chosen from therein.
}
  \item{ivar}{
index of the rationing part. If the object is of class \dQuote{alr} the column names are chosen from therein. If not and ivar is not provided by
the user, it is assumed that the rationing part was the last column of the data in the simplex.
}
  \item{useClassInfo}{
if FALSE, the class information of object \code{x} is not used.
}
}
\details{
The function allows also to preserve absolute values when class info is provided. Otherwise only the relative information is preserved.
}
\value{
the transformed data matrix
}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\author{
Matthias Templ
}
\seealso{
\code{\link{isomLRinv}}, \code{\link{cenLRinv}}, \code{\link{cenLR}}, \code{\link{addLR}}
}
\examples{
data(arcticLake)
x <- arcticLake
x.alr <- addLR(x, 2)
y <- addLRinv(x.alr)
## This exactly fulfills:
addLRinv(addLR(x, 3))
data(expenditures)
x <- expenditures
y <- addLRinv(addLR(x, 5))
head(x)
head(y)
## --> absolute values are preserved as well.

## preserve only the ratios:
addLRinv(x.alr, ivar=2, useClassInfo=FALSE)


}
\keyword{ manip }