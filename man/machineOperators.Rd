\name{machineOperators}
\alias{machineOperators}
\docType{data}
\title{ Machine operators data set}
\description{
The data set from Aitchison (1986), p. 382, contains 
compositions of eight-hour shifts of 27 machine operators. 
The parts represent proportions of shifts in each activity: 
high-quality production, low-quality production, machine 
setting and machine repair.
  
}
\usage{data(machineOperators)}
\format{
  A data frame with 27 observations on the following 4 variables.
  \describe{
    \item{\code{hqproduction}}{high-quality production}
    \item{\code{lqproduction}}{low-quality production}
    \item{\code{setting}}{machine settings}
    \item{\code{repair}}{machine repair}
  }
}
\references{ Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p. 
}
\examples{
## maybe str(machineOperators) ; plot(machineOperators) ...
}
\keyword{datasets}
