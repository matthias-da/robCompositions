########################################
###   ilr coordinates:   ###############
########################################

#ilr coordinates of original, independent and interaction table using SBP1 and SBP2

ilr.2x2 <- function(x, margin=3){
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
  class(res) <- "ilr2x2"
  res	
}
