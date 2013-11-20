\name{ilr}
\alias{ilr}
\title{ Isometric log-ratio transformation }
\description{
  An isometric log-ratio transformation with a special choice of the balances according to Hron et al. (2010).
}
\usage{
ilr(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class data.frame or matrix with positive entries}
}
\details{
The ilr transformation moves D-part compositional data from the simplex into a (D-1)-dimensional real space isometrically.
  From this choice of the balances, all the relative information of the part \eqn{x_1} 
  from the remaining parts is separated. It is useful for estimating missing values in 
  \eqn{x_1} by regression of the remaining variables.
}
\value{
  The isomLR transformed data.
}
\references{ Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and
C. Barcel'o-Vidal (2003) Isometric logratio transformations for
compositional data analysis. \emph{Mathematical Geology}, \bold{35}(3)
279-300. \\

Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods 
\emph{Computational Statistics and Data Analysis}, vol 54 (12), pages 3095-3107.   
 }
\author{ Karel Hron, Matthias Templ }
\seealso{ \code{\link{isomLR}}, \code{\link{isomLRinv}},\code{\link[compositions]{ilr}} }
\examples{
## function is deprecated, use isomLR instead.
}
\keyword{ math }
