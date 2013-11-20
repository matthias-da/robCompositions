\name{lmCoDaX}
\alias{lmCoDaX}
\alias{ilrregression}
\alias{robilrregression}
\title{
 Classical and robust regression of non-compositional response on compositional predictors
}
\description{
Delivers appropriate inference for regression of y on a compositional matrix X.
}
\usage{
lmCoDaX(y, X, method = "robust")
ilrregression(X, y)
robilrregression(X, y)
}
\arguments{
  \item{y}{
The response which should be non-compositional
}
  \item{X}{
The compositional predictors as a matrix, data.frame or numeric vector
}
  \item{method}{
If robust, LTS-regression is applied, while with method equals \dQuote{classical},
the conventional least squares regression is applied.
}
}
\details{
Compositional explanatory variables should not be directly used in a linear regression model
because any inference statistic can become misleading. While various approaches for this
problem were proposed, here an approach based on the isometric logratio (ilr) transformation
is used. 
}
\value{
An object of class \sQuote{lts} or \sQuote{lm} and two summary objects.
}
\references{
 Filzmoser, P., Hron, K., Thompsonc, K. (2012)
Linear regression with compositional explanatory variables. 
\emph{Journal of Applied Statistics}, 39, 1115-1128.
}
\author{
Peter Filzmoser
}
\seealso{
\code{\link{lm}}
}
\examples{
## How the total household expenditures in EU Member
## States depend on relative contributions of 
## single household expenditures:
data(expendituresEU)
y <- as.numeric(apply(expendituresEU,1,sum))
lmCoDaX(y, expendituresEU, method="classical")
lmCoDaX(y, expendituresEU, method="robust")
}
\keyword{ models }