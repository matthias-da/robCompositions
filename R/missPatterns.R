
missPatterns <- function(x){
	# identification of the missing pattern structure 
	# Matthias Templ, Oct 10, 2011
	if(is.null(dim(x))) stop("the data set has to be consist of at least two variables")
	
	w <- is.na(x)
	tmp <- ifelse(is.na(x), 1, 0)  # 'ifelse' does not omit 'dim' attribute
	tmpC <- apply(tmp, 1, paste, collapse=":")
	tab <- table(tmpC)
	tabcomb <- sapply(names(tab), 
			function(x) as.integer(unlist(strsplit(x, ":", fixed=TRUE))), 
			USE.NAMES=FALSE)
	tabcomb <- if(is.null(dim(tabcomb))) as.matrix(tabcomb) else t(tabcomb)
	tabcomb <- ifelse(tabcomb==0,TRUE,FALSE)
	cn <- names(tab)
	groups <- sapply(cn, function(y){
				(which(tmpC %in% y))
			})
	## Karels beiden MUSS-Variablen ;-):
	csum <- lapply(groups, length)
	amountComb <- cbind(data.frame(tabcomb), csum=as.numeric(csum))
	rsum <- apply(w, 1, sum)
	## TODO: N variable dazu, + 2. zeilenweise, spaltenweise
	list(groups=groups, cn=cn, tabcomb=tabcomb, tabcombPlus=amountComb, rsum=rsum)
}

zeroPatterns <- function(x){
	# identification of the zero pattern structure 
	# Matthias Templ, Oct 10, 2011
	if(is.null(dim(x))) stop("the data set has to be consist of at least two variables")
	
	w <- x == 0
	tmp <- ifelse(x==0, 1, 0)  # 'ifelse' does not omit 'dim' attribute
	tmpC <- apply(tmp, 1, paste, collapse=":")
	tab <- table(tmpC)
	tabcomb <- sapply(names(tab), 
			function(x) as.integer(unlist(strsplit(x, ":", fixed=TRUE))), 
			USE.NAMES=FALSE)
	tabcomb <- if(is.null(dim(tabcomb))) as.matrix(tabcomb) else t(tabcomb)
	tabcomb <- ifelse(tabcomb==0,TRUE,FALSE)
	cn <- names(tab)
	groups <- sapply(cn, function(y){
				(which(tmpC %in% y))
			})
	## Karels beiden MUSS-Variablen ;-):
	csum <- lapply(groups, length)
	amountComb <- cbind(data.frame(tabcomb), csum=as.numeric(csum))
	rsum <- apply(w, 1, sum)
	## TODO: N variable dazu, + 2. zeilenweise, spaltenweise
	list(groups=groups, cn=cn, tabcomb=tabcomb, tabcombPlus=amountComb, rsum=rsum)
}
