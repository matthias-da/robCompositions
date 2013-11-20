\name{phd}
\alias{phd}
\docType{data}
\title{ PhD Students in the EU }
\description{
PhD students in Europe based on the standard klassification system splitted by different kind of 
studies (given as percentages).
}
\usage{data(phd)}
\format{
  The format is:
 num [1:33, 1:8] 516.5 7.5 5.2 22.6 4.8 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:33] "EU" "Belgien" "Bulgarien" "Tschech.Rep." ...
  ..$ : chr [1:8] "Gesamtzahl der Doktoranden (in 1 000)" "maennlich" "weiblich" "Naturwissen-schaften, Mathematik, Informatik u. Ingenieurwesen" ...
}
\details{
Due to unknown reasons the rowSums of the percentages is not always 100.
}
\source{
\url{http://epp.eurostat.ec.europa.eu/cache/ITY_PUBLIC/1-18092009-AP/DE/1-18092009-AP-DE.PDF}
}
\examples{
data(phd)
phdImputed <- impCoda(phd)$xOrig
}
\keyword{datasets}
