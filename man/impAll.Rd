\name{impAll}
\alias{impAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replacement of rounded zeros and missing values.
}
\description{
Parametric replacement of rounded zeros and missing values for compositional data using
classical and robust methods based on ilr-transformations with special choice of balances.
Values under detection limit should be saved with the negative value of the detection limit (per variable). Missing values should be coded as NA. 
}
\usage{
impAll(x)
}
\arguments{
  \item{x}{
data frame
}
}
\details{
This is a wrapper function that calls \emph{impRZilr()} for the replacement of zeros and \emph{impCoda} for the imputation of missing values sequentially.
The detection limit is automatically derived form negative numbers in the data set.
}
\value{
The imputed data set.
}
\references{ 
Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods, 
\emph{Computational Statistics and Data Analysis}, vol 54 (12), pages 3095-3107.  

Martin-Fernandez, J.A.  and Hron, K. and Templ, M. and Filzmoser, P. and Palarea-Albaladejo, J. (2012)  
Model-based replacement of rounded zeros in compositional data: Classical and robust approaches,
\emph{Computational Statistics}, 56 (2012), S. 2688 - 2704.
 }
\author{
Jiri Eichler
}
\note{
This function is mainly used by the compositionsGUI.
}
\seealso{
\code{\link{impCoda}}, \code{\link{impRZilr}}
}
\examples{
## see the compositionsGUI
}
\keyword{ manip }
