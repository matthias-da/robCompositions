\name{impRZalr}
\alias{impRZalr}
\title{ alr EM-based Imputation for Rounded Zeros }
\description{
  A modified EM alr-algorithm for replacing rounded zeros in
compositional data sets.
}
\usage{
impRZalr(x, pos = ncol(x), dl = rep(0.05, ncol(x) - 1), eps = 1e-04, 
   maxit = 50, bruteforce=FALSE, method="lm", step=FALSE, 
   nComp = "boot", R = 10, verbose = FALSE)
}
\arguments{
  \item{x}{ Compositional data }
  \item{pos}{ Position of the rationing variable for alr transformation }
  \item{dl}{ Detection limit for each part }
  \item{eps}{ convergence criteria }
  \item{maxit}{ maximum number of iterations }
  \item{bruteforce}{ if TRUE, imputations over dl are set to dl. If FALSE, truncated (Tobit) regression is applied.  }
  \item{method}{ either \dQuote{lm} (default) or \dQuote{MM} }
  \item{step}{ if TRUE, a stepwise (AIC) procedure is applied when fitting models }    
  \item{nComp}{ if determined, it fixes the number of pls components. If \dQuote{boot}, the number of pls components are estimated using
a bootstraped cross validation approach. }  
  \item{R}{ number of bootstrap samples for the determination of pls components. Only important for method \dQuote{pls}. }  
  \item{verbose}{ additional print output during calculations. }  
}
\details{
Statistical analysis of compositional data including zeros runs into problems, because log-ratios cannot be applied. 
Usually, rounded zeros are considerer as missing not at random missing values.
The algorithm first applies an additive log-ratio transformation to the compositions. Then the rounded zeros are imputed
using a modified EM algorithm.
}
\value{
  \item{xOrig }{Original data frame or matrix}
  \item{xImp }{Imputed data}
  \item{wind }{Index of the missing values in the data}
  \item{iter }{Number of iterations}
  \item{eps }{eps}
}
\author{ Matthias Templ and Karel Hron }
\seealso{ \code{\link{impRZilr}} }
\examples{
data(arcticLake)
x <- arcticLake
## generate rounded zeros artificially:
x[x[,1] < 5, 1] <- 0
x[x[,2] < 47, 2] <- 0
xia <- impRZalr(x, pos=3, dl=c(5,47), eps=0.05)
xia$xImp
}
\keyword{ manip }
\keyword{ multivariate }
