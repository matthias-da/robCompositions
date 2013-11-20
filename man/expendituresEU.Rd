\name{expendituresEU}
\alias{expendituresEU}
\docType{data}
\title{
Mean consumption expenditures data.
}
\description{
Mean consumption expenditure of households at EU-level. 
The final consumption expenditure of households encompasses all domestic costs 
(by residents and non-residents) for individual needs.
}
\usage{data(expendituresEU)}
\format{
  A data frame with 27 observations on the following 12 variables.
  \describe{
    \item{\code{Food}}{a numeric vector}
    \item{\code{Alcohol}}{a numeric vector}
    \item{\code{Clothing}}{a numeric vector}
    \item{\code{Housing}}{a numeric vector}
    \item{\code{Furnishings}}{a numeric vector}
    \item{\code{Health}}{a numeric vector}
    \item{\code{Transport}}{a numeric vector}
    \item{\code{Communications}}{a numeric vector}
    \item{\code{Recreation}}{a numeric vector}
    \item{\code{Education}}{a numeric vector}
    \item{\code{Restaurants}}{a numeric vector}
    \item{\code{Other}}{a numeric vector}
  }
}
\source{
Eurostat: 
\url{http://epp.eurostat.ec.europa.eu/statistics_explained/images/c/c2/Mean_consumption_expenditure_of_households,_2005(PPS).PNG}
}
\references{
Eurostat provides a website with the data:

\url{http://epp.eurostat.ec.europa.eu/statistics_explained/index.php/Household_consumption_expenditure}
}
\examples{
data(expendituresEU)
}
\keyword{datasets}
