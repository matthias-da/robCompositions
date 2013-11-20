\name{cenLR}
\alias{cenLR}
\title{
Centred log-ratio transformation
}
\description{
The cenLR transformation moves D-part compositional data from the simplex into a D-dimensional real space.
}
\usage{
cenLR(x)
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
\code{\link{cenLRinv}}, \code{\link{addLR}}, \code{\link{isomLR}}, \code{\link{addLRinv}}, 
\code{\link{isomLRinv}}
}
\examples{
data(expenditures)
eclr <- cenLR(expenditures)
inveclr <- cenLRinv(eclr)
head(expenditures)
head(inveclr)
head(isomLRinv(eclr$x.clr))
}
\keyword{ manip }