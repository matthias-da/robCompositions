# orthbasis2 <- function(D){
# 	V <- matrix(0,nrow=D,ncol=D-1)
# 	for (i in 1:ncol(V)){
# 		V[1:i,i] <- 1/i
# 		V[i+1,i] <- (-1)
# 		V[,i] <- V[,i]*sqrt(i/(i+1))
# 	}
# 	return(V)
# }
# 
# # [,1] [,2] [,3] [,4]
# # [1,]   -1    0    0    0
# # [2,]    1   -1    0    0
# # [3,]    1    1   -1    0
# # [4,]    1    1    1   -1
# # [5,]    1    1    1    1
# 
# b <- function(D){
#   m <- matrix(1,nrow=D,ncol=D-1)
#   for (i in 1:(ncol(m)-1)){
#     m[1:i,i+1] <- 0
#     m[i,i] <- -1
#   }
#   m[i+1,i+1] <- -1
#   m
# }
# 
# b(D)



#' Orthonormal basis
#' 
#' Orthonormal basis from cenLR transformed data to pivotCoord transformated data.
#' 
#' For the chosen balances for \dQuote{pivotCoord}, this is the orthonormal basis
#' that transfers the data from centered logratio to isometric logratio.
#' 
#' @param D number of parts (variables)
#' @return the orthonormal basis.
#' @author Karel Hron, Matthias Templ. Some code lines of this function are a copy 
#' from function gsi.buildilr from 
## package compositions. The authors are 
## Raimon Tolosana-Delgado, K.Gerald v.d. Boogaart
## and the intellecutal properties belongs to them.
#' @seealso \code{\link{pivotCoord}}, \code{\link{cenLR}}
#' @keywords manip
#' @export
#' @examples
#' 
#' data(expenditures)
#' V <- orthbasis(ncol(expenditures))
#' xcen <- cenLR(expenditures)$x.clr
#' xi <- as.matrix(xcen) %*% V$V
#' xi
#' xi2 <- pivotCoord(expenditures)
#' xi2
orthbasis <- function(D){
	ilrBase <- NULL
	## 1/-1/0 matrix
	b <- function(D){
	  m <- matrix(1,nrow=D,ncol=D-1)
	  for (i in 1:(ncol(m)-1)){
	    m[1:i,i+1] <- 0
	    m[i,i] <- -1
	  }
	  m[i+1,i+1] <- -1
	  m
	}
	transform <- function(basis){
		## most of the code lines of this function are a copy of function gsi.buildilr* from 
		## package compositions. The authors are 
		## Raimon Tolosana-Delgado, K.Gerald v.d. Boogaart
		## and the intellecutal properties belongs to them.
			basis <- as.matrix(basis)
			nc <- ncol(basis); D <- nrow(basis)
			isPos <- basis > 0; isNeg <- basis < 0
			nPos <- matrix(1, D, D) %*% isPos
			nNeg <- matrix(1, D, D) %*% isNeg
			basis <- (isPos * nNeg - isNeg * nPos)
			numb <- sapply(1:nc, function(i) {
						1 / sqrt(basis[,i] %*% basis[,i]) 
					})
			numb <- matrix(numb, ncol = nc, nrow = D, byrow = TRUE)
			basis <- basis * numb
			return(basis)
	}
	basis <- b(D)
	# codes <- matrix(rep(0,(D-1)*D),ncol=D)
	# for(i in 1:(D-1)){
	# 	for(j in 1:D){
	# 		codes[i,] <- c(rep(0,i-1),-1,rep(1,D-i))
	# 	}
	# }   
	basis <- basis * (-1)
	V <- transform(basis)
	ll <- list("V" = V, "basisv" = basis)
	return(ll)
}

