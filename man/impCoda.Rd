\name{impCoda}
\alias{impCoda}
\title{ Imputation of missing values in compositional data }
\description{
  This function offers different methods for the imputation of missing values in 
  compositional data. Missing values are initialized with proper values. 
  Then iterative algorithms try to find better estimations for the former missing values.
}
\usage{
impCoda(x, maxit = 10, eps = 0.5, method = "ltsReg", closed = FALSE, 
  init = "KNN", k = 5, dl = rep(0.05, ncol(x)), 
  noise=0.1, bruteforce=FALSE)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{maxit}{ maximum number of iterations }
  \item{eps}{ convergence criteria }
  \item{method}{ imputation method }
  \item{closed}{ imputation of transformed data (using ilr transformation) or in the original space (\code{closed} equals TRUE)  }
  \item{init}{ method for initializing missing values }
  \item{k}{ number of nearest neighbors (if init $==$ \dQuote{KNN}) }
  \item{dl}{ detection limit(s), only important for the imputation of rounded zeros }
  \item{noise}{ amount of adding random noise to predictors after convergency }  
  \item{bruteforce}{ if TRUE, imputations over dl are set to dl. If FALSE, truncated (Tobit) regression is applied. }
}
\details{
  eps: The algorithm is finished as soon as the imputed values stabilize, i.e. until the sum of Aitchison distances from the present and previous iteration changes only marginally (eps).\\
  
  method: Several different methods can be chosen, such as 
  \sQuote{ltsReg}: least trimmed squares regression is used within the iterative procedure.
  \sQuote{lm}: least squares regression is used within the iterative procedure. 
  \sQuote{classical}: principal component analysis is used within the 
  iterative procedure. 
  \sQuote{ltsReg2}: least trimmed squares regression is used within the iterative procedure. 
    The imputated values are perturbed in the direction of the predictor by values drawn 
    form a normal distribution with mean and standard deviation related to the corresponding 
    residuals and multiplied by \code{noise}.
    
  method \sQuote{roundedZero} is experimental. It imputes rounded zeros within our iterative framework.   
}
\value{
  \item{xOrig }{Original data frame or matrix}
  \item{xImp }{Imputed data}
  \item{criteria }{Sum of the Aitchison distances from the present and previous iteration}
  \item{iter }{Number of iterations}
  \item{maxit }{Maximum number of iterations }
  \item{w }{Amount of imputed values}
  \item{wind }{Index of the missing values in the data}
}
\references{ 
Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods 
\emph{Computational Statistics and Data Analysis}, vol 54 (12), pages 3095-3107.  
 }
\author{ Matthias Templ, Karel Hron }
\seealso{ \code{\link{impKNNa}}, \code{\link{isomLR}} }
\examples{
data(expenditures)
x <- expenditures
x[1,3]
x[1,3] <- NA
xi <- impCoda(x)$xImp
xi[1,3]
s1 <- sum(x[1,-3])
impS <- sum(xi[1,-3])
xi[,3] * s1/impS
}
\keyword{ robust }
\keyword{ multivariate }
\keyword{ iteration }
