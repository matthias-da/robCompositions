\name{daFisher}
\alias{daFisher}
\title{
Discriminant analysis by Fisher Rule.
}
\description{
Discriminant analysis by Fishers rule.
}
\usage{
daFisher(x, grp, coda = TRUE, method = "classical", plotScore=FALSE)
}
\arguments{
  \item{x}{
a matrix or data frame containing the explanatory variables
          (training set)
}
  \item{grp}{
grouping variable: a factor specifying the class for each
          observation.
}
  \item{coda}{
TRUE, when the underlying data are compositions.
}
  \item{method}{
\dQuote{classical} or \dQuote{robust}
}
  \item{plotScore}{
TRUE, if the scores should be plotted automatically.
}
}
\details{
The Fisher rule leads only to linear boundaries. However, this method allows for dimension reduction and thus
for a better visualization of the separation boundaries. For the Fisher discriminant rule (Fisher, 1938; Rao, 1948) the
assumption of normal distribution of the groups is not explicitly required, although the method looses its optimality
in case of deviations from normality.

The classical Fisher discriminant rule is invariant to ilr and clr transformations. The robust rule is invariant
to ilr transformations if affine equivariant robust estimators of location and covariance are taken.

Robustification is done (method \dQuote{robust}) by estimating the columnwise means and the covariance
by the Minimum Covariance Estimator.
}
\value{
an object of class \dQuote{daFisher} including the following elements
\item{B }{Between variance of the groups}
\item{W }{Within variance of the groups}
\item{loadings}{loadings}
\item{coda}{coda}
}
\references{
Filzmoser, P. and Hron, K. and Templ, M. (2009) Discriminant analysis for compositional data
and robust parameter estimation. \emph{Research Report SM-2009-3}, Vienna University of Technology, 27 pages.

Fisher, R. A. (1938) The statistical utiliziation of multiple measurements. \emph{Annals of Eugenics}, 8:376-386.

Rao, C.R. (1948) The utilization of multiple measurements in problems of biological classification.
\emph{Journal of the Royal Statistical Society}, Series B, 10:159-203.
}
\author{
The code is was written by Peter Filzmoser. Minor modifications by Matthias Templ.
}
\seealso{
\code{\link[rrcov]{Linda}}
}
\examples{
require(MASS)
x1 <- mvrnorm(20,c(0,0,0),diag(3))
x2 <- mvrnorm(30,c(3,0,0),diag(3))
x3 <- mvrnorm(40,c(0,3,0),diag(3))
X <- rbind(x1,x2,x3)
grp=c(rep(1,20),rep(2,30),rep(3,40))

#par(mfrow=c(1,2))
d1 <- daFisher(X,grp=grp,method="classical",coda=FALSE)
d2 <- daFisher(X,grp=grp,method="robust",coda=FALSE)
d2
}
\keyword{ multivariate }