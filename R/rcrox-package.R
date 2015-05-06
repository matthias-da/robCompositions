

#' Artic lake sediment data
#' 
#' Sand, silt, clay compositions of 39 sediment samples at different water
#' depths in an Arctic lake.  This data set can be found on page 359 of the
#' Aitchison book (see reference).
#' 
#' The rows sum up to 100, except for rounding errors.
#' 
#' @name arcticLake
#' @docType data
#' @format A data frame with 39 observations on the following 3 variables.
#' \describe{ \item{list("sand")}{numeric vector of percentages of sand}
#' \item{list("silt")}{numeric vector of percentages of silt}
#' \item{list("clay")}{numeric vector of percentages of clay} }
#' @source Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
#' Data} Monographs on Statistics and Applied Probability. Chapman \& Hall
#' Ltd., London (UK). 416p.
#' @keywords datasets
#' @examples
#' 
#' data(arcticLake)
#' 
NULL





#' Coffee data
#' 
#' 27 commercially available coffee samples of different origins.
#' 
#' In the original data set, 15 volatile compounds (descriptors of coffee
#' aroma) were selected for a statistical analysis. We selected only three
#' compounds (compositional parts) Hydroxy-2-propanone, methylpyrazine and
#' methylfurfural to allow for a visualization in a ternary diagram.
#' 
#' @name coffee
#' @docType data
#' @format A data frame with 27 observations on the following 4 variables.
#' \describe{ \item{list("Metpyr")}{Hydroxy-2-propanone}
#' \item{list("5-Met")}{methylpyrazine} \item{list("furfu")}{methylfurfural}
#' \item{list("sort")}{a character vector} }
#' @references M.~Korhonov\'a, K.~Hron, D.~Klimc\'ikov\'a, L.~Muller,
#' P.~Bedn\'ar, and P.~Bart\'ak (2009) Coffee aroma - statistical analysis of
#' compositional data. \emph{Talanta}, 80(2): 710--715.
#' @keywords datasets
#' @examples
#' 
#' data(coffee)
#' 
NULL





#' Household expenditures data
#' 
#' This data set from Aitchison (1986), p. 395, describes household
#' expenditures (in former Hong Kong dollars) on five commundity groups.
#' 
#' This data set contains household expenditures on five commodity groups of 20
#' single men. The variables represent housing (including fuel and light),
#' foodstuff, alcohol and tobacco, other goods (including clothing, footwear
#' and durable goods) and services (including transport and vehicles). Thus
#' they represent the ratios of the men's income spent on the mentioned
#' expenditures.
#' 
#' @name expenditures
#' @docType data
#' @format A data frame with 20 observations with the following 5 variables.
#' \describe{ \item{list("housing")}{housing (including fuel and light)}
#' \item{list("foodstuffs")}{foodstuffs} \item{list("alcohol")}{alcohol and
#' tobacco} \item{list("other")}{other goods (including clothing, footwear and
#' durable goods)} \item{list("services")}{services (including transport and
#' vehicles)} }
#' @source Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
#' Data} Monographs on Statistics and Applied Probability. Chapman \& Hall
#' Ltd., London (UK). 416p.
#' @keywords datasets
#' @examples
#' 
#' data(expenditures)
#' ## imputing a missing value in the data set using k-nearest neighbor imputation:
#' expenditures[1,3]
#' expenditures[1,3] <- NA
#' impKNNa(expenditures)$xImp[1,3]
#' 
NULL





#' Mean consumption expenditures data.
#' 
#' Mean consumption expenditure of households at EU-level.  The final
#' consumption expenditure of households encompasses all domestic costs (by
#' residents and non-residents) for individual needs.
#' 
#' 
#' @name expendituresEU
#' @docType data
#' @format A data frame with 27 observations on the following 12 variables.
#' \describe{ \item{list("Food")}{a numeric vector} \item{list("Alcohol")}{a
#' numeric vector} \item{list("Clothing")}{a numeric vector}
#' \item{list("Housing")}{a numeric vector} \item{list("Furnishings")}{a
#' numeric vector} \item{list("Health")}{a numeric vector}
#' \item{list("Transport")}{a numeric vector} \item{list("Communications")}{a
#' numeric vector} \item{list("Recreation")}{a numeric vector}
#' \item{list("Education")}{a numeric vector} \item{list("Restaurants")}{a
#' numeric vector} \item{list("Other")}{a numeric vector} }
#' @references Eurostat provides a website with the data:
#' 
#' \url{http://epp.eurostat.ec.europa.eu/statistics_explained/index.php/Household_consumption_expenditure}
#' @source Eurostat:
#' \url{http://epp.eurostat.ec.europa.eu/statistics_explained/images/c/c2/Mean_consumption_expenditure_of_households,_2005(PPS).PNG}
#' @keywords datasets
#' @examples
#' 
#' data(expendituresEU)
#' 
NULL





#' Haplogroups data.
#' 
#' Distribution of European Y-chromosome DNA (Y-DNA) haplogroups by region in
#' percentage.
#' 
#' Human Y-chromosome DNA can be divided in genealogical groups sharing a
#' common ancestor, called haplogroups.
#' 
#' @name haplogroups
#' @docType data
#' @format A data frame with 38 observations on the following 12 variables.
#' \describe{ \item{list("I1")}{pre-Germanic (Nordic)}
#' \item{list("I2b")}{pre-Celto-Germanic} \item{list("I2a1")}{Sardinian,
#' Basque} \item{list("I2a2")}{Dinaric, Danubian}
#' \item{list("N1c1")}{Uralo-Finnic, Baltic, Siberian}
#' \item{list("R1a")}{Balto-Slavic, Mycenaean Greek, Macedonia}
#' \item{list("R1b")}{Italic, Celtic, Germanic; Hitite, Armenian}
#' \item{list("G2a")}{Caucasian, Greco-Anatolien} \item{list("E1b1b")}{North
#' and Eastern Afrika, Near Eastern, Balkanic} \item{list("J2")}{Mesopotamian,
#' Minoan Greek, Phoenician} \item{list("J1")}{Semitic (Arabic, Jewish)}
#' \item{list("T")}{Near-Eastern, Egyptian, Ethiopian, Arabic} }
#' @source Eupedia:
#' \url{http://www.eupedia.com/europe/european_y-dna_haplogroups.shtml}
#' @keywords datasets
#' @examples
#' 
#' data(haplogroups)
#' 
NULL





#' Machine operators data set
#' 
#' The data set from Aitchison (1986), p. 382, contains compositions of
#' eight-hour shifts of 27 machine operators.  The parts represent proportions
#' of shifts in each activity: high-quality production, low-quality production,
#' machine setting and machine repair.
#' 
#' 
#' @name machineOperators
#' @docType data
#' @format A data frame with 27 observations on the following 4 variables.
#' \describe{ \item{list("hqproduction")}{high-quality production}
#' \item{list("lqproduction")}{low-quality production}
#' \item{list("setting")}{machine settings} \item{list("repair")}{machine
#' repair} }
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman \& Hall Ltd., London (UK). 416p.
#' @keywords datasets
#' @examples
#' 
#' ## maybe str(machineOperators) ; plot(machineOperators) ...
#' 
NULL





#' PhD Students in the EU
#' 
#' PhD students in Europe based on the standard klassification system splitted
#' by different kind of studies (given as percentages).
#' 
#' Due to unknown reasons the rowSums of the percentages is not always 100.
#' 
#' @name phd
#' @docType data
#' @format The format is: num [1:33, 1:8] 516.5 7.5 5.2 22.6 4.8 ...  - attr(*,
#' "dimnames")=List of 2 ..$ : chr [1:33] "EU" "Belgien" "Bulgarien"
#' "Tschech.Rep." ...  ..$ : chr [1:8] "Gesamtzahl der Doktoranden (in 1 000)"
#' "maennlich" "weiblich" "Naturwissen-schaften, Mathematik, Informatik u.
#' Ingenieurwesen" ...
#' @source
#' \url{http://epp.eurostat.ec.europa.eu/cache/ITY_PUBLIC/1-18092009-AP/DE/1-18092009-AP-DE.PDF}
#' @keywords datasets
#' @examples
#' 
#' data(phd)
#' phdImputed <- impCoda(phd)$xOrig
#' 
NULL





#' Robust Estimation for Compositional Data.
#' 
#' The package contains methods for imputation of compositional data including
#' robust methods, (robust) outlier detection for compositional data, (robust)
#' principal component analysis for compositional data, (robust) factor
#' analysis for compositional data, (robust) discriminant analysis (Fisher
#' rule) and (robust) Anderson-Darling normality tests for compositional data
#' as well as popular log-ratio transformations (alr, clr, ilr, and their
#' inverse transformations).
#' 
#' \tabular{ll}{ Package: \tab robCompositions\cr Type: \tab Package\cr
#' Version: \tab 1.3.3\cr Date: \tab 2009-11-28\cr License: \tab GPL 2\cr
#' LazyLoad: \tab yes\cr }
#' 
#' @name robCompositions-package
#' @aliases robCompositions-package robCompositions
#' @docType package
#' @author Matthias Templ, Peter Filzmoser, Karel Hron,
#' 
#' Maintainer: Matthias Templ <templ@@tuwien.ac.at>
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman \& Hall Ltd., London (UK). 416p. \
#' 
#' Filzmoser, P., and Hron, K. (2008) Outlier detection for compositional data
#' using robust methods. \emph{Math. Geosciences}, \bold{40} 233-248.
#' 
#' Filzmoser, P., Hron, K., Reimann, C. (2009) Principal Component Analysis for
#' Compositional Data with Outliers. \emph{Environmetrics}, \bold{20} (6),
#' 621--632.
#' 
#' P. Filzmoser, K. Hron, C. Reimann, R. Garrett (2009): Robust Factor Analysis
#' for Compositional Data.  \emph{Computers and Geosciences}, \bold{35} (9),
#' 1854--1861.
#' 
#' Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values
#' for compositional data using classical and robust methods
#' \emph{Computational Statistics and Data Analysis}, \bold{54} (12),
#' 3095--3107.
#' 
#' C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter (2008): Statistical
#' Data Analysis Explained.  \emph{Applied Environmental Statistics with R}.
#' John Wiley and Sons, Chichester, 2008.
#' @keywords package
#' @examples
#' 
#' ## k nearest neighbor imputation
#' data(expenditures)
#' expenditures[1,3]
#' expenditures[1,3] <- NA
#' impKNNa(expenditures)$xImp[1,3]
#' 
#' ## iterative model based imputation
#' data(expenditures)
#' x <- expenditures
#' x[1,3]
#' x[1,3] <- NA
#' xi <- impCoda(x)$xImp
#' xi[1,3]
#' s1 <- sum(x[1,-3])
#' impS <- sum(xi[1,-3])
#' xi[,3] * s1/impS
#' 
#' xi <- impKNNa(expenditures)
#' xi
#' summary(xi)
#' \dontrun{plot(xi, which=1)}
#' plot(xi, which=2)
#' plot(xi, which=3)
#' 
#' ## pca
#' data(expenditures)
#' p1 <- pcaCoDa(expenditures)
#' p1
#' plot(p1)
#' 
#' ## outlier detection
#' data(expenditures)
#' oD <- outCoDa(expenditures)
#' oD
#' plot(oD)
#' 
#' ## transformations
#' data(arcticLake)
#' x <- arcticLake
#' x.alr <- addLR(x, 2)
#' y <- addLRinv(x.alr)
#' addLRinv(addLR(x, 3))
#' data(expenditures)
#' x <- expenditures
#' y <- addLRinv(addLR(x, 5))
#' head(x)
#' head(y)
#' addLRinv(x.alr, ivar=2, useClassInfo=FALSE)
#' 
#' data(expenditures)
#' eclr <- cenLR(expenditures)
#' inveclr <- cenLRinv(eclr)
#' head(expenditures)
#' head(inveclr)
#' head(cenLRinv(eclr$x.clr))
#' 
#' require(MASS)
#' Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)
#' z <- isomLRinv(mvrnorm(100, mu=c(0,2), Sigma=Sigma))
#' 
NULL





#' Aphyric skye lavas data
#' 
#' AFM compositions of 23 aphyric Skye lavas. This data set can be found on
#' page 360 of the Aitchison book (see reference).
#' 
#' 
#' @name skyeLavas
#' @docType data
#' @format A data frame with 23 observations on the following 3 variables.
#' \describe{ \item{list("sodium-potassium")}{a numeric vector of percentages
#' of Na2O\eqn{+}K2O} \item{list("iron")}{a numeric vector of percentages of
#' Fe2O3} \item{list("magnesium")}{a numeric vector of percentages of MgO} }
#' @source Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
#' Data} Monographs on Statistics and Applied Probability. Chapman \& Hall
#' Ltd., London (UK). 416p.
#' @keywords datasets
#' @examples
#' 
#' data(skyeLavas)
#' 
NULL



