\name{robVariation}
\alias{robVariation}
\title{ Robust variation matrix }
\description{
  Estimates the variation matrix with robust methods.
}
\usage{
robVariation(x, robust=TRUE)
}
\arguments{
  \item{x}{ data frame or matrix with positive entries }
  \item{robust}{ if FALSE, standard measures are used. }
}
\details{
  The variation matrix is estimated for a given compositional data set. Instead of using the classical standard deviations the 
  \code{\link[stats]{mad}} is used when parameter robust is set to TRUE.
}
\value{
 The (robust) variation matrix.
}
\references{ Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p. }
\author{ Matthias Templ }
\examples{
data(expenditures)
robVariation(expenditures)
robVariation(expenditures, robust=FALSE)
}
\keyword{ multivariate }
\keyword{ robust }
