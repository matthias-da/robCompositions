`robVariation` <-
function(x, robust=TRUE){
    rvars <- matrix(0, ncol=ncol(x), nrow=ncol(x))
	if(robust){
    for( i in 1:ncol(x)){
      for( j in 1:ncol(x)){
        if( i < j ) rvars[i,j] <- (mad(log(x[,i]/x[,j])))^2
      }
    }
	} else{
		for( i in 1:ncol(x)){
			for( j in 1:ncol(x)){
				if( i < j ) rvars[i,j] <- (var(log(x[,i]/x[,j])))
			}
		}		
	}
	rvars[lower.tri(rvars)] <- rvars[upper.tri(rvars)]
    return(rvars) 
}

