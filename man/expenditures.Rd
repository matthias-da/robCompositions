\name{expenditures}
\alias{expenditures}
\docType{data}
\title{ Household expenditures data }
\description{
  This data set from Aitchison (1986), p. 395, describes household expenditures (in former Hong Kong dollars) on five commundity groups. 
}
\usage{data(expenditures)}
\format{
  A data frame with 20 observations with the following 5 variables.
  \describe{
    \item{\code{housing}}{housing (including fuel and light)}
    \item{\code{foodstuffs}}{foodstuffs}
    \item{\code{alcohol}}{alcohol and tobacco}
    \item{\code{other}}{other goods (including clothing, footwear and durable goods)}
    \item{\code{services}}{services (including transport and vehicles)}
  }
}
\details{
  This data set contains household expenditures on five commodity groups of 20 single men. The
variables represent housing (including fuel and light), foodstuff, alcohol and tobacco, other goods (including
clothing, footwear and durable goods) and services (including transport and vehicles). Thus they represent
the ratios of the men's income spent on the mentioned expenditures. 
}
\source{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\examples{
data(expenditures)
## imputing a missing value in the data set using k-nearest neighbor imputation:
expenditures[1,3]
expenditures[1,3] <- NA
impKNNa(expenditures)$xImp[1,3]
}
\keyword{datasets}
