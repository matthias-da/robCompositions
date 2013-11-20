\name{adjust}
\alias{adjust}
\title{ Adjusting for original scale }
\description{
  Results from the model based iterative methods provides the results in another scale (but the 
  ratios are still the same). This function rescale the output to the original scale.
}
\usage{
adjust(x)
}
\arguments{
  \item{x}{ object from class \sQuote{imp} }
}
\details{
It is self-explaining if you try the examples.
}
\value{
The object of class \sQuote{imp} but with the adjusted imputed data.
}
\references{ 
Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods 
\emph{Computational Statistics and Data Analysis}, In Press, Corrected Proof, ISSN: 0167-9473, DOI:10.1016/j.csda.2009.11.023  
 }
\author{ Matthias Templ }
\seealso{ \code{\link{impCoda}} }
\examples{
data(expenditures)
x <- expenditures
x[1,3] <- x[2,4] <- x[3,3] <- x[3,4] <- NA
xi <- impCoda(x)
x
xi$xImp
adjust(xi)$xImp
}
\keyword{ manip }