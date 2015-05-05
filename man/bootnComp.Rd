\name{bootnComp}
\alias{bootnComp}
\title{
Bootstrap to find optimal number of components
}
\description{
Combined bootstrap and cross validation procedure to find optimal number of PLS components 
}
\usage{
bootnComp(X, y, R = 99, plotting = FALSE)
}
\arguments{
  \item{X}{
predictors as a matrix
}
  \item{y}{
response
}
  \item{R}{
number of bootstrap samples
}
  \item{plotting}{
if TRUE, a diagnostic plot is drawn for each bootstrap replicate
}
}
\details{
Heavily used internally in function impRZilr.
}
\value{
Including other information in a list, the optimal number of components
}
\author{
Matthias Templ
}
\seealso{
\code{\link{impRZilr}}
}
\examples{
## we refer to impRZilr()
}
\keyword{ manip }
