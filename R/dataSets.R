#' Arctic lake sediment data
#' 
#' Sand, silt, clay compositions of 39 sediment samples at different water depths in an Arctic lake.
#' This data set can be found on page 359 of the Aitchison book (see reference).
#'
#' \itemize{
#'  \item{\code{sand }}{numeric vector of percentages of sand}
#'  \item{\code{silt }}{numeric vector of percentages of silt}
#'  \item{\code{clay }}{numeric vector of percentages of clay}
#' }
#'
#' @name arcticLake
#' @docType data
#' @usage data(arcticLake)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @details The rows sum up to 100, except for rounding errors. 
#' @format A data frame with 39 rows and 3 variables
#' @references Aitchison, J. (1986). \emph{The Statistical Analysis of Compositional Data}. Monographs on Statistics and Applied Probability. Chapman \& Hall Ltd., London (UK). 416p. 
#' @keywords data
#' @examples 
#' 
#' data(arcticLake)
#' str(arcticLake)
#' summary(arcticLake)
#' rowSums(arcticLake)
NULL



#' coffee data set
#' 
#' 27 commercially available coffee samples of different origins.
#'
#' \itemize{
#' \item{\code{Metpyr }}{Hydroxy-2-propanone}
#' \item{\code{5-Met }}{methylpyrazine}
#' \item{\code{furfu }}{methylfurfural}
#' \item{\code{sort }}{a character vector}
#' }
#'
#' @name coffee
#' @docType data
#' @usage data(coffee)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}, Karel Hron
#' @details In the original data set, 15 volatile compounds (descriptors of coffee aroma) were selected for a statistical analysis. We selected only three compounds (compositional parts) hydroxy-2-propanone, methylpyrazine and methylfurfural to allow for a visualization in a ternary diagram.
#' @format A data frame with 27 observations on the following 4 variables.
#' @references M. Korhonov\'a, K. Hron, D. Klimc\'ikov\'a, L. Muller, P. Bedn\'ar, and P. Bart\'ak (2009). Coffee aroma - statistical analysis of compositional data. \emph{Talanta}, 80(2): 710--715.
#' @keywords data
#' @examples 
#' 
#' data(coffee)
#' str(coffee)
#' summary(coffee)
NULL

#' machine operators 
#' 
#' Compositions of eight-hour shifts of 27 machine operators
#'
#' \itemize{
#' \item{\code{hqproduction }}{high-quality production}
#' \item{\code{lqproduction }}{low-quality production}
#' \item{\code{setting }}{machine settings}
#' \item{\code{repair }}{machine repair}
#' }
#'
#' @name machineOperators
#' @docType data
#' @usage data(machineOperators)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @details The data set from Aitchison (1986), p. 382, contains compositions of eight-hour shifts of 27 machine operators. The parts represent proportions of shifts in each activity:  high-quality production, low-quality production, machine setting and machine repair.
#' @format A data frame with 27 observations on the following 4 variables.
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional Data} Monographs on Statistics and Applied Probability. Chapman \& Hall Ltd., London (UK). 416p. 
#' @keywords data
#' @examples 
#' 
#' data(machineOperators)
#' str(machineOperators)
#' summary(machineOperators)
#' rowSums(machineOperators)
NULL


#' Aphyric skye lavas data 
#' 
#' AFM compositions of 23 aphyric Skye lavas. This data set can be found on page 360 of the Aitchison book (see reference).
#'
#' \itemize{
#' \item{\code{sodium-potassium }}{a numeric vector of percentages of Na2O\eqn{+}K2O}
#' \item{\code{iron }}{a numeric vector of percentages of Fe2O3}
#' \item{\code{magnesium }}{a numeric vector of percentages of MgO}
#' }
#'
#' @name skyeLavas
#' @docType data
#' @usage data(skyeLavas)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A data frame with 23 observations on the following 3 variables.
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional Data} Monographs on Statistics and Applied Probability. Chapman \& Hall Ltd., London (UK). 416p. 
#' @keywords data
#' @examples 
#' 
#' data(skyeLavas)
#' str(skyeLavas)
#' summary(skyeLavas)
#' rowSums(skyeLavas)
NULL


#' Synthetic household expenditures toy data set
#' 
#' This data set from Aitchison (1986), p. 395, describes household expenditures (in former Hong Kong dollars) on five commundity groups. 
#'
#' \itemize{
#' \item{\code{housing }}{housing (including fuel and light)}
#' \item{\code{foodstuffs }}{foodstuffs}
#' \item{\code{alcohol }}{alcohol and tobacco}
#' \item{\code{other }}{other goods (including clothing, footwear and durable goods)}
#' \item{\code{services }}{services (including transport and vehicles)}
#' }
#'
#' @name expenditures
#' @docType data
#' @usage data(expenditures)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}, Karel Hron
#' @details This data set contains household expenditures on five commodity groups of 20 single men. The variables represent housing (including fuel and light), foodstuff, alcohol and tobacco, other goods (including clothing, footwear and durable goods) and services (including transport and vehicles). Thus they represent the ratios of the men's income spent on the mentioned expenditures. 
#' @format A data frame with 20 observations on the following 5 variables.
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional Data} Monographs on Statistics and Applied Probability. Chapman \& Hall Ltd., London (UK). 416p. 
#' @keywords data
#' @examples 
#' 
#' data(expenditures)
#' ## imputing a missing value in the data set using k-nearest neighbor imputation:
#' expenditures[1,3]
#' expenditures[1,3] <- NA
#' impKNNa(expenditures)$xImp[1,3]
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
#' \itemize{ 
#' \item{\code{Food}}{a numeric vector} 
#' \item{\code{Alcohol}}{a numeric vector} 
#' \item{\code{Clothing}}{a numeric vector}
#' \item{\code{Housing}}{a numeric vector} 
#' \item{\code{Furnishings}}{a numeric vector} 
#' \item{\code{Health}}{a numeric vector}
#' \item{\code{Transport}}{a numeric vector} 
#' \item{\code{Communications}}{a numeric vector} 
#' \item{\code{Recreation}}{a numeric vector}
#' \item{\code{Education}}{a numeric vector} 
#' \item{\code{Restaurants}}{a numeric vector} 
#' \item{\code{Other}}{a numeric vector} 
#' }
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
#' \itemize{ 
#' \item{\code{I1 }}{pre-Germanic (Nordic)}
#' \item{\code{I2b }}{pre-Celto-Germanic} 
#' \item{\code{I2a1 }}{Sardinian, Basque} 
#' \item{\code{I2a2 }}{Dinaric, Danubian}
#' \item{\code{N1c1 }}{Uralo-Finnic, Baltic, Siberian}
#' \item{\code{R1a }}{Balto-Slavic, Mycenaean Greek, Macedonia}
#' \item{\code{R1b }}{Italic, Celtic, Germanic; Hitite, Armenian}
#' \item{\code{G2a }}{Caucasian, Greco-Anatolien} 
#' \item{\code{E1b1b }}{North and Eastern Afrika, Near Eastern, Balkanic} 
#' \item{\code{J2 }}{Mesopotamian, Minoan Greek, Phoenician} 
#' \item{\code{J1 }}{Semitic (Arabic, Jewish)}
#' \item{\code{T }}{Near-Eastern, Egyptian, Ethiopian, Arabic} 
#' }
#' @source Eupedia:
#' \url{http://www.eupedia.com/europe/european_y-dna_haplogroups.shtml}
#' @keywords datasets
#' @examples
#' 
#' data(haplogroups)
#' 
NULL


#' PhD Students in the EU
#' 
#' PhD students in Europe based on the standard klassification system splitted
#' by different kind of studies (given as percentages).
#' 
#' Due to unknown reasons the rowSums of the percentages is not always 100.
#' 
#' \itemize{
#' \item{\code{total }}{total phd students (in 1.000)}                            
#' \item{\code{male }}{male phd students (in 1.000)}                                      
#' \item{\code{female }}{total phd students (in 1.000)}                                     
#' \item{\code{technical }}{phd students in natural and technical sciences}
#' \item{\code{socio-economic-low }}{phd students in social sciences, economic sciences and law sciences}                    
#' \item{\code{human }}{phd students in human sciences including teaching}
#' \item{\code{health }}{phd students in health and life sciences}               
#' \item{\code{agriculture }}{phd students in agriculture} 
#' }
#' 
#' @name phd
#' @docType data
#' @format A data set on 33 compositions and 8 variables.
#' @source
#' \url{http://epp.eurostat.ec.europa.eu/cache/ITY_PUBLIC/1-18092009-AP/DE/1-18092009-AP-DE.PDF}
#' @keywords datasets
#' @examples
#' 
#' data(phd)
#' phdImputed <- impCoda(phd)$xOrig
#' 
NULL



