\name{alr}
\alias{alr}
\title{
Additive log-ratio transformation
}
\description{
The alr transformation moves D-part compositional data from the simplex into a (D-1)-dimensional real space.
}
\usage{
alr(x, ivar=ncol(x))
}
\arguments{
  \item{x}{D-part compositional data}
  \item{ivar}{Rationing part}
}
\details{
The compositional parts are divided by the rationing part before the logarithm is taken. 
}
\value{
A list of class \dQuote{alr} which includes the following content:
\item{x.alr}{the transformed data}
\item{varx}{the rationing variable}
\item{ivar}{the index of the rationing variable, indicating the column number of the rationing variable in the data matrix \emph{x}}
\item{cnames}{the column names of \emph{x}}
The additional information such as \emph{cnames} or \emph{ivar} is usefull when a back-transformation is applied on the \sQuote{same} data set.
}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\author{
Matthias Templ
}
\seealso{
\code{\link{addLR}}, \code{\link{addLRinv}}, \code{\link[compositions]{alr}}
}
\examples{
## function is deprecated, use addLR instead.
}
\keyword{ manip }