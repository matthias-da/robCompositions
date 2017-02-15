#' Independence table
#' 
#' Estimates the expected frequencies from an m-way table under the 
#' null hypotheses of independence.
#' 
#' @param x an object of class table
#' @param margin determines how the margins of the table should be estimated (default via geometric mean margins)
#' @param frequency indicates whether absolute or relative frequencies should be computed.
#' @param pTabMethod to estimate the propability table. Default is \sQuote{dirichlet}. Other available methods: 
#' \sQuote{classical} that is function \code{prop.table()} from package base or method \dQuote{half} that add 1/2 to each cell
#' to avoid zero problems.
#' @details Because of the compositional nature of probability tables, the independence tables should 
#' be estimated using geometric margins.
#' @author Matthias Templ
#' @return The independence table(s) with either relative or absolute frequencies.
#' @references 
#' Juan Jose Egozcuea, Vera Pawlowsky-Glahn, Matthias Templ, Karel Hron (2015)
#' Independence in Contingency Tables Using Simplicial Geometry. 
#' \emph{Communications in Statistics - Theory and Methods}, Vol. 44 (18), 3978--3996.
#' DOI:10.1080/03610926.2013.824980
#' 
#' @export
#' @examples 
#' data(precipitation) 
#' tab1 <- indTab(precipitation)
#' tab1
#' sum(tab1)
#' 
#' \dontrun{
#' data("PreSex", package = "vcd")
#' indTab(PreSex)
#' }
indTab <-
  function (x, margin = c("gmean_sum", "sum"), frequency = c("relative", "absolute"), 
            pTabMethod = c("dirichlet", "half", "classical")) 
  {
    margin <- match.arg(margin)
    frequency <- match.arg(frequency)
    pTabMethod <- match.arg(pTabMethod)
    n <- sum(x)
    #	x <- x/n
    x <- pTab(x, method=pTabMethod)
    d <- dim(x)
    margins <- lapply(1:length(d), function(i) apply(x, i, margin))
    tab <- array(apply(expand.grid(margins), 1, prod), d, dimnames = dimnames(x))
    if (frequency == "relative") 
      res <- tab/sum(tab)
    else res <- tab*n
    res
  }

