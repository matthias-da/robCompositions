\name{impRZilr}
\alias{impRZilr}
\title{
EM-based replacement of rounded zeros in compositional data
}
\description{
Parametric replacement of rounded zeros for compositional data using
classical and robust methods based on ilr-transformations with special choice of balances.
}
\usage{
impRZilr(x, maxit = 10, eps = 0.1, method = "roundedZero", dl = rep(0.05, ncol(x)), bruteforce = FALSE)
}
\arguments{
  \item{x}{
data.frame or matrix
}
  \item{maxit}{
maximum number of iterations
}
  \item{eps}{
convergency criteria 
}
  \item{method}{
either \dQuote{roundedZeor} or \dQuote{roundedZeroRobust}
}
  \item{dl}{
Detection limit for each variable
}
  \item{bruteforce}{
sets imputed values above the detection limit to the detection limit. Replacement above the detection limit are only exeptionally occur due to numerical instabilities. The default is FALSE!
}
}
\details{
Statistical analysis of compositional data including zeros runs into problems, because log-ratios cannot be applied. 
Usually, rounded zeros are considerer as missing not at random missing values.

The algorithm iteratively imputes parts with rounded zeros whereas in each step (1) an specific ilr transformation is applied (2) tobit regression is applied (3) the rounded zeros are replaced by the expected values
(4) the corresponding inverse ilr transformation is applied. After all parts are imputed, the algorithm starts again until the imputations do not change.
}
\value{
  \item{xOrig }{Original data frame or matrix}
  \item{xImp }{Imputed data}
  \item{wind }{Index of the missing values in the data}
  \item{iter }{Number of iterations}
  \item{eps }{eps}
}
\author{
Matthias Templ and Peter Filzmoser
}
\seealso{
\code{\link{impRZalr}}
}
\examples{
data(arcticLake)
x <- arcticLake
## generate rounded zeros artificially:
x[x[,1] < 5, 1] <- 0
x[x[,2] < 47, 2] <- 0
xia <- impRZilr(x, dl=c(5,47,9999), eps=0.05)
xia$xImp
}
\keyword{ manip }
\keyword{ multivariate }
