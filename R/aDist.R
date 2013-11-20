`aDist` <-
  function(x, y){
      if(is.vector(x)) x <- matrix(x, ncol=length(x))
	  if(is.vector(y)) y <- matrix(y, ncol=length(y))	  
	  
	  matOrig <- as.numeric(t(x))
	  matImp <- as.numeric(t(y))
	  n <- dim(x)[1]
	  p <- dim(x)[2]
	  dims <- as.integer(c(n, p))
	  rowDists <-  as.numeric(rep(0.0, n))
	  distance <- as.numeric(0.0)
	  out <- .C("da", 
				  matOrig,
				  matImp,
				  dims,
				  rowDists,
				  distance,
				  PACKAGE="robCompositions", NUOK=TRUE
		  )[[5]]
	  return(out)
}	  
	  
	