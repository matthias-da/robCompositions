\name{pcaCoDa}
\alias{pcaCoDa}
\title{ Robust principal component analysis for compositional data }
\description{
This function applies robust principal component analysis for compositional data.
}
\usage{
pcaCoDa(x, method = "robust")
}
\arguments{
  \item{x}{ compositional data }
  \item{method}{ either \dQuote{robust} (default) or \dQuote{standard}}
}
\details{
The compositional data set is transformed using the ilr tranformation. 
Afterwards, robust principal component analysis is performed. 
Resulting loadings and scores are back-transformed to the clr space where 
the compositional biplot can be shown. 
}
\value{
  \item{scores }{scores in clr space}
  \item{loadings }{loadings in clr space}
  \item{eigenvalues }{eigenvalues of the clr covariance matrix}
  \item{method }{method}
  \item{princompOutputClr }{output of \code{princomp} needed in \code{plot.pcaCoDa}}
}
\references{ 
 Filzmoser, P., Hron, K., Reimann, C. (2009)
 Principal Component Analysis for Compositional Data with Outliers. \emph{Environmetrics}, \bold{20}, 621-632.
}
\author{ K. Hron, P. Filzmoser, M. Templ }
\seealso{\code{\link{print.pcaCoDa}}, \code{\link{plot.pcaCoDa}} }
\examples{
data(expenditures)
p1 <- pcaCoDa(expenditures)
p1
plot(p1)
}
\keyword{ multivariate }
