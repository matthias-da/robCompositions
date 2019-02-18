#' Propability table
#' 
#' Calculates the propability table using different methods
#' 
#' @param x an object of class table
#' @param method default is \sQuote{dirichlet}. Other available methods: 
#' \sQuote{classical} that is function \code{prop.table()} from package base or method \dQuote{half} that add 1/2 to each cell
#' to avoid zero problems.
#' @param alpha constant used for method \sQuote{dirichlet}
#' @author Matthias Templ
#' @return The probablity table
#' @references 
#' Egozcue, J.J., Pawlowsky-Glahn, V., Templ, M., Hron, K. (2015)
#' Independence in contingency tables using simplicial geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, 44 (18), 3978--3996.
#' 
#' @export
#' @examples 
#' data(precipitation) 
#' pTab(precipitation)
#' pTab(precipitation, method = "dirichlet")
pTab <- function(x, method="dirichlet", alpha=1/length(as.numeric(x))){
	if(method == "half"){
	   res <- (x + 1/2)/(sum(x)+nrow(x)*ncol(x)/2)
    }
	if(method == "classical"){
	   res <-	prop.table(x)
	}
	if(method == "dirichlet"){
		res <- ((x + alpha))/(sum(x) + sum(alpha))
	}	
	res <- res / sum(res)
    res
}
