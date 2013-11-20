\name{adtest}
\alias{adtest}
\title{ Anderson-Darling Normality Tests }
\description{
This function provides three kinds of Anderson-Darling Normality Tests 
(Anderson and Darling, 1952). 
}
\usage{
adtest(x, R = 1000, locscatt = "standard")
}
\arguments{
  \item{x}{ either a numeric vector, or a data.frame, or a matrix }
  \item{R}{ Number of Monte Carlo simulations to obtain p-values}
  \item{locscatt}{ standard for classical estimates of mean and (co)variance. robust for 
                   robust estimates using \sQuote{covMcd()} from package robustbase}
}
\details{
Three version of the test are implemented (univariate, angle and radius test) and it depends on 
the data which test is chosen.

If the data is univariate the univariate Anderson-Darling test for normality is applied.

If the data is bivariate the angle Anderson-Darling test for normality is performed out.

If the data is multivariate the radius Anderson-Darling test for normality is used.

If \sQuote{locscatt} is equal to \dQuote{robust} then within the procedure, robust estimates
of mean and covariance are provided using \sQuote{covMcd()} from package robustbase.

To provide estimates for the corresponding p-values, i.e. to compute the probability of obtaining
a result at least as extreme as the one that was actually observed under the null
hypothesis, we use Monte Carlo techniques where we check how often the statistic of 
the underlying data is more extreme than statistics obtained from simulated normal distributed data
with the same (column-wise-) mean(s) and (co)variance.
}
\value{
  \item{statistic }{The result of the corresponding test statistic}
  \item{method }{The chosen method (univariate, angle or radius)}
  \item{p.value }{p-value}
}
\references{ 
Anderson, T.W. and Darling, D.A. (1952) 
  Asymptotic theory of certain goodness-of-fit criteria based 
  on stochastic processes. \emph{Annals of Mathematical Statistics}, \bold{23}  
  193-212. 
}
\author{ Karel Hron, Matthias Templ }
\note{ 
These functions are use by \code{\link{adtestWrapper}}. 
}
\seealso{ \code{\link{adtestWrapper}} }
\examples{
adtest(rnorm(100))
data(machineOperators)
x <- machineOperators
adtest(isomLR(x[,1:2]))
adtest(isomLR(x[,1:3]))
adtest(isomLR(x))
adtest(isomLR(x[,1:2]), locscatt="robust")
}
\keyword{ htest }