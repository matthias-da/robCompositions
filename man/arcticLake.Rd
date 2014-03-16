\name{arcticLake}
\alias{arcticLake}
\docType{data}
\title{ Artic lake sediment data }
\description{
  Sand, silt, clay compositions of 39 sediment samples at different water depths in an Arctic lake.
  This data set can be found on page 359 of the Aitchison book (see reference).
}
\usage{data(arcticLake)}
\format{
  A data frame with 39 observations on the following 3 variables.
  \describe{
    \item{\code{sand}}{numeric vector of percentages of sand}
    \item{\code{silt}}{numeric vector of percentages of silt}
    \item{\code{clay}}{numeric vector of percentages of clay}
  }
}
\details{
 The rows sum up to 100, except for rounding errors. 
}
\source{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\examples{
data(arcticLake)
}
\keyword{datasets}
