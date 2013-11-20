\name{coffee}
\alias{coffee}
\docType{data}
\title{
Coffee data
}
\description{
27 commercially available coffee samples of different origins.
}
\usage{data(coffee)}
\format{
  A data frame with 27 observations on the following 4 variables.
  \describe{
    \item{\code{Metpyr}}{Hydroxy-2-propanone}
    \item{\code{5-Met}}{methylpyrazine}
    \item{\code{furfu}}{methylfurfural}
    \item{\code{sort}}{a character vector}
  }   
  
}
\details{
In the original data set, 15 volatile compounds (descriptors of
coffee aroma) were selected for a statistical analysis. We selected only three compounds (compositional parts) Hydroxy-2-propanone, 
methylpyrazine and methylfurfural to allow for a visualization in a ternary diagram.
}
\references{
M.~Korhonov\'a, K.~Hron, D.~Klimc\'ikov\'a, L.~Muller, P.~Bedn\'ar, and
  P.~Bart\'ak (2009)
Coffee aroma - statistical analysis of compositional data.
\emph{Talanta}, 80(2): 710--715.
}
\examples{
data(coffee)
}
\keyword{datasets}