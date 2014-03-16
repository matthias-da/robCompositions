\name{aDist}
\alias{aDist}
\title{Aitchison distance}
\description{
  Computes the Aitchison distance between two observations or between two data sets.
}
\usage{
aDist(x, y)
}
\arguments{
  \item{x}{ a vector, matrix or data.frame }
  \item{y}{ a vector, matrix or data.frame with equal dimension as \code{x} }
}
\details{
  This distance measure accounts for the relative scale property of the 
  Aitchison distance. It measures the distance between two compositions if \code{x} and \code{y} 
  are vectors and evaluate sum of the distances between \code{x} and \code{y} for each row of \code{x} and \code{y} if
  \code{x} and \code{y} are matrices or data frames.
  
  It is to compare two matrices.
  
  The underlying code is written in C and allows a fast computation also for large data sets.
}
\value{
  The Aitchison distance between two compositions or between two data sets.
}
\references{ Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman and
Hall Ltd., London (UK). 416p. 

Aitchison, J. and Barcelo-Vidal, C. and Martin-Fernandez, J.A. and Pawlowsky-Glahn, V. (2000) Logratio analysis and 
compositional distance. \emph{Mathematical Geology}, \bold{32}, 271-275.

Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods 
\emph{Computational Statistics and Data Analysis}, vol 54 (12), pages 3095-3107.  
}
\author{ Matthias Templ }
\seealso{ \code{\link{isomLR}} }
\examples{
data(expenditures)
x <- xOrig <- expenditures
## Aitchison distance between the first 2 observations:
aDist(x[,1], x[,2])

## set some missing values:
x[1,3] <- x[3,5] <- x[2,4] <- x[5,3] <- x[8,3] <- NA

## impute them:
xImp <- impCoda(x, method="ltsReg")$xImp

## calculate the relative Aitchsion distance between xOrig and xImp:
aDist(xOrig, xImp)
}
\keyword{ math }
\keyword{ arith }
