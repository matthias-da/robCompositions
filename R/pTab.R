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
#' Juan Jose Egozcuea, Vera Pawlowsky-Glahn, Matthias Templ, Karel Hron (2015)
#' Independence in Contingency Tables Using Simplicial Geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, Vol. 44 (18), 3978--3996.
#' DOI:10.1080/03610926.2013.824980
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