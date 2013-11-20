impAll <-
function(x) {
	maxLimits = apply(x, 2, min, na.rm = TRUE)
	maxLimits = -maxLimits
	maxLimits[maxLimits < 0] = 0

	if(any(is.na(x))) {
		temp <- x
		for(i in 1:length(maxLimits)) {
			temp[na.omit(temp[i]) < 0, i] = maxLimits[i] * 2 / 3
		}

		res <- adjust(impCoda(temp))
		isna = is.na(x)
		x[isna] = res$xImp[isna]
	}
	if(any(x < 0)) {
		temp <- x
		temp[temp < 0] = 0

		res <- impRZilr(temp, dl=maxLimits)
		x = res$xImp
	}

	return(x)
}

