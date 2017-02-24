#' ilr coordinates in 2x2 tables
#' 
#' ilr coordinates of original, independent and interaction table using SBP1 and SBP2
#' 
#' @param x a 2x2 table
#' @author Kamila Facevicova, Matthias Templ
#' @return The ilr coordinates
#' @references 
#' K. Facevicova, K. Hron, V. Todorov, D. Guo, M. Templ (2014).
#' Logratio approach to statistical analysis of 2x2 compositional tables.
#' \emph{Journal of Applied Statistics}, Volume 41 (5), 944--958.
#' DOI:10.1080/02664763.2013.856871
#' @export
#' @examples 
#' data(employment) 
#' ilr.2x2(employment[,,"AUT"])
#' ilr.2x2(employment)

ilr.2x2 <- function(x){
  ilrind <- function(x){
    z1 <- z2 <- numeric(3)
    z1[1] <- (1/3^(1/2))*log(x[1,1]/x[2,2])
    z1[2] <- (2^(1/2)/(4*3^(1/2)))*log((x[1,1]*x[2,1]^3)/(x[1,2]^3*x[2,2]))
    z1[3] <- (1/(2*2^(1/2)))*log((x[1,1]*x[1,2])/(x[2,1]*x[1,1]))
    z2[1] <- (1/2)*log((x[1,1]*x[2,2])/(x[2,1]*x[1,2]))
    z2[2] <- (1/2^(1/2))*log(x[2,1]/x[1,2])
    list(z1=z1, z2=z2)
  }
  ilrint <- function(x){
    z2 <- (1/2)*log((x[1,1]*x[2,2])/(x[2,1]*x[1,2]))
    z1 <- numeric(3)
    z1[1] <- (3^(1/2)/6)*log((x[1,1]*x[2,2])/(x[2,1]*x[1,2]))
    z1[2] <- (6^(1/2)/12)*log((x[2,1]*x[1,2])/(x[1,1]*x[2,2]))
    z1[3] <- (2^(1/2)/4)*log((x[2,1]*x[1,2])/(x[1,1]*x[2,2]))
    list(z1=z1, z2=z2)
  }
  ilrorig <- function(x){
    z1 <- z2 <- numeric(3)
    z1[1] <- (3^(1/2)/2)*log(x[1,1]/(x[2,1]*x[1,2]*x[2,2])^(1/3))
    z1[2] <- (2/3)^(1/2)*log(x[2,1]/(x[1,2]*x[2,2])^(1/2))
    z1[3] <- 1/2^(1/2)*log(x[1,2]/x[2,2])
    z2[1] <- 1/2*log((x[1,1]*x[2,2])/(x[2,1]*x[1,2]))
    z2[2] <- 1/2^(1/2)*log(x[2,1]/x[1,2])
    z2[3] <- 1/2^(1/2)*log(x[1,1]/x[2,2])
    list(z1=z1, z2=z2)
  }
  ilrmethod <- function(x, type) {
    switch(type,
           ilrind = ilrind(x),
           ilrint = ilrint(x),
           ilrorig = ilrorig(x))
  }
  method <- ifelse(class(x) == "ind2x2", "ilrind", 
                   ifelse(class(x) == "int2x2", "ilrint",
                          "ilrorig"))
  dn <- dimnames(x)
  if(length(dim(x)) == 2){
    res <- ilrmethod(x, method)
  } else {
    res <- apply(x, margin, ilrmethod, type=method)
  }
#  class(res) <- "ilr2x2"
  res	
}
