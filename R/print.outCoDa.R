print.outCoDa <- function(x, ...){
 cat("\n --------------------\n")	
 print(paste(length(which(x$outlierIndex == TRUE)), "out of", length(x$outlierIndex), "observations are detected as outliers."))
 cat("\n --------------------\n\n")		
}