\name{pcaCoDa}
\alias{pcaCoDa}
\title{ Robust principal component analysis for compositional data }
\description{
This function applies robust principal component analysis for compositional data.
}
\usage{
pcaCoDa(x, method = "robust", mult_comp=NULL)
}
\arguments{
  \item{x}{ compositional data }
  \item{method}{ either \dQuote{robust} (default) or \dQuote{standard}}
  \item{mult_comp}{a list of numeric vectors holding the indices of linked compositions}
}
\details{
The compositional data set is transformed using the ilr tranformation. 
Afterwards, robust principal component analysis is performed. 
Resulting loadings and scores are back-transformed to the clr space where 
the compositional biplot can be shown. 

\code{mult_comp} is used when there are more than one group of compositional parts in the data.
To give an illustrative example, lets assume that one variable group measures angles of the inner ear-bones 
of animals which sum up to 100 and another one having percentages of a whole on
the thickness of the inner ear-bones included. Then two groups of variables exists which are both compositional parts.
The ilr-transformation is then internally applied to each group independently whenever the \code{mult_comp} is set 
correctly.
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

## just for illustration how to set the mult_comp argument
p1 <- pcaCoDa(expenditures, mult_comp=list(c(1,2,3),c(4,5)))

}
\keyword{ multivariate }
