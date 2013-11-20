\name{clr}
\alias{clr}
\title{
Centred log-ratio transformation
}
\description{
The clr transformation moves D-part compositional data from the simplex into a D-dimensional real space.
}
\usage{
clr(x)
}
\arguments{
  \item{x}{
multivariate data ideally of class data.frame or matrix
}
}
\details{
Each composition is divided by the geometric mean of its parts before the logarithm is taken. 
}
\value{
The transformed data, including
\item{x.clr}{clr transformed data}
\item{gm}{the geometric means of the original composition.}
}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\author{
Matthias Templ
}
\note{
The resulting transformed data set is singular by definition.
}
\seealso{
\code{\link{cenLR}}, \code{\link{cenLRinv}}, \code{\link[compositions]{clr}}
}
\examples{
## function is deprecated, use cenLR instead.
}
\keyword{ manip }