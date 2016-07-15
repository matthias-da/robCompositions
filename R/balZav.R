#' New average symmetric coordinates
#' 
#' 
#' @param x compositional data
#' @return \item{Z.av}{A matrix of new average symmetric coordinates.}
#' @author Petra Kynclova



balZav <- function(x){
  D <- ncol(x)
  Z.av <- matrix(NA,ncol=2,nrow=nrow(x))
  p1 <- sqrt(D-1+sqrt(D*(D-2)))/sqrt(2*D)

  #if(D==2)  stop average calculation...
  if(D==3){
    p2 <- x[,3]
  }
  else{
    p2 <- apply(x[,3:D],1,prod)
  }
  
  p3 <- (sqrt(D-2)+sqrt(D))/(sqrt(D-2)*(D-1+sqrt(D*(D-2))))
  p4 <- 1/(D-1+sqrt(D*(D-2)))
  Z.av[,1] <- p1*(log(x[,1]/(x[,2]^p4 * p2^p3)))
  Z.av[,2] <- p1*(log(x[,2]/(x[,1]^p4 * p2^p3)))
  return(Z.av)
}