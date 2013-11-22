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
impRZilr(x, maxit = 10, eps = 0.1, method = "pls", 
   dl = rep(0.05, ncol(x)), nComp=NULL, bruteforce = FALSE,
   noisemethod="residuals", noise=FALSE, R=10, verbose=FALSE)
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
either \dQuote{lm}, \dQuote{MM} or \dQuote{pls}
}
  \item{dl}{
Detection limit for each variable
}
  \item{nComp}{
if determined, it fixes the number of pls components. If NULL, the number of pls components are estimated.
}
  \item{bruteforce}{
sets imputed values above the detection limit to the detection limit. Replacement above the detection limit are only exeptionally occur due to numerical instabilities. The default is FALSE!
}
  \item{noisemethod}{
  adding noise to imputed values. Experimental
  }
  \item{noise}{
  TRUE to activate noise (experimental)
  }
  \item{R}{
  number of bootstrap samples for the determination of pls components. Only important for method \dQuote{pls}.
  }
  \item{verbose}{
  additional print output during calculations.
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
  \item{wind }{Index of the missing values / values below detection limit in the data}
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
#xia <- impRZilr(x, dl=c(5,47,9999), eps=0.05, method="lm")
#xia$xImp
}
\keyword{ manip }
\keyword{ multivariate }
