#' child, middle and eldery population
#' 
#' Percentages of childs, middle generation and eldery population in 195 countries.
#'
#' \itemize{
#'  \item{\code{<15 }}{Percentage of people with age below 15}
#'  \item{\code{15-60 }}{Percentage of people with age between 15 and 60}
#'  \item{\code{60+ }}{Percentage of people with age above 60}
#'  \item{\code{country }}{country of origin}
#' }
#'
#' @name ageCatWorld
#' @docType data
#' @usage data(ageCatWorld)
#' @author extracted by Karel Hron and Eva Fiserova, implemented by Matthias Templ
#' @details The rows sum up to 100. 
#' @format A data frame with 195 rows and 4 variables
#' @references Fiserova, E. and Hron, K. (2012). Statistical Inference in Orthogonal Regression for Three-Part Compositional Data Using a Linear Model with Type-II Constraints. \emph{Communications in Statistics - Theory and Methods}, 41 (13-14), 2367-2385. 
#' @keywords data
#' @examples 
#' 
#' data(ageCatWorld)
#' str(ageCatWorld)
#' summary(ageCatWorld)
#' rowSums(ageCatWorld[, 1:3])
#' ternaryDiag(ageCatWorld[, 1:3])
#' plot(pivotCoord(ageCatWorld[, 1:3]))
NULL


#' regional alcohol per capita (15+) consumption by WHO region
#'
#' \itemize{
#'  \item{\code{country }}{Country}
#'  \item{\code{year }}{Year}
#'  \item{\code{recorded }}{Recorded alcohol consumption}
#'  \item{\code{unrecorded }}{Unrecorded alcohol consumption}
#' }
#'
#' @name alcoholreg
#' @docType data
#' @usage data(alcoholreg)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @source Transfered from the World Health Organisation website. 
#' @format A data frame with 6 rows and 4 variables
#' @keywords data
#' @examples 
#' 
#' data("alcoholreg")
#' alcoholreg
NULL


#' alcohol consumptions by country and type of alcohol
#'
#' \itemize{
#'  \item{\code{country }}{Country}
#'  \item{\code{year }}{Year}
#'  \item{\code{beer }}{Consumption of pure alcohol on beer (in percentages)}
#'  \item{\code{wine }}{Consumption of pure alcohol on wine (in percentages)}
#'  \item{\code{spirits }}{Consumption of pure alcohol on spirits (in percentages)}
#'  \item{\code{other }}{Consumption of pure alcohol on other beverages (in percentages)}
#' }
#'
#' @name alcohol
#' @docType data
#' @usage data(alcohol)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @source Transfered from the World Health Organisation website. 
#' @format A data frame with 193 rows and 6 variables
#' @keywords data
#' @examples 
#' 
#' data("alcohol")
#' str(alcohol)
#' summary(alcohol)
NULL

#' arctic lake sediment data
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
#' ternaryDiag(arcticLake)
#' plot(pivotCoord(arcticLake))
NULL


#' hospital discharges on cancer and distribution of age
#' 
#' Hospital discharges of in-patients on neoplasms (cancer) per 100.000 inhabitants (year 2007) and population age structure. 
#' 
#' \itemize{
#' \item{\code{country }}{country}                            
#' \item{\code{year }}{year}                                      
#' \item{\code{p1 }}{percentage of population with age below 15}                       
#' \item{\code{p2 }}{percentage of population with age between 15 and 60}
#' \item{\code{p3 }}{percentage of population with age above 60}                    
#' \item{\code{discharges }}{hospital discharges of in-patients on neoplasms (cancer) per 100.000 inhabitants}
#' }
#' 
#' @name cancer
#' @docType data
#' @details The response (discharges) is provided for the European Union countries (except Greece, Hungary and Malta) by Eurostat. As explanatory variables we use the age structure of the population in the same countries (year 2008). The age structure consists of three parts, age smaller than 15, age between 15 and 60 and age above 60 years, and they are expressed as percentages on the overall population in the countries. The data are provided by the United Nations Statistics Division.
#' @format A data set on 24 compositions on 6 variables.
#' @source
#' \url{http://www.ec.europa.eu/eurostat} and \url{http://unstats.un.org/unsd}
#' @keywords datasets
#' @author conversion to R by Karel Hron and Matthias Templ \email{matthias.templ@@tuwien.ac.at} 
#' @references K. Hron, P. Filzmoser, K. Thompson (2012). Linear regression with compositional explanatory variables. \emph{Journal of Applied Statistics}, Volume 39, Issue 5, 2012. 
#' @examples
#' 
#' data(cancer)
#' str(cancer)
NULL


#' malignant neoplasms cancer
#' 
#' Two main types of malignant neoplasms cancer affecting colon and lung, respectively, in male and female populations. 
#' For this purpose population data (2012) from 35 OECD countries were collected.
#' 
#' \itemize{
#' \item{\code{country }}{country}                                  
#' \item{\code{females-colon }}{number of colon cancer cases in female population}                       
#' \item{\code{females-lung }}{number of lung cancer cases in female population}                    
#' \item{\code{males-colon }}{number of colon cancer cases in male population}
#' \item{\code{males-lung }}{number of lung cancer cases in male population}
#' }
#' 
#' @name cancerMN
#' @docType data
#' @details The data are obtained from the OECD website.
#' @format A data set on 35 compositional tables on 4 parts (row-wise sorted cells) and 5 variables.
#' @source
#' \url{http://www.oecd.org}
#' @keywords datasets
#' @author conversion to R by Karel Hron and intergration by Matthias Templ \email{matthias.templ@@tuwien.ac.at} 
#' @examples
#' 
#' data(cancerMN)
#' head(cancerMN)
#' rowSums(cancerMN[, 2:5])
NULL


#' C-horizon of the Kola data with rounded zeros
#' 
#' This data set is almost the same as \code{\link[mvoutlier]{chorizon}} data set
#' in package \code{mvoutlier} and \code{\link[VIM]{chorizonDL}}, except that values below the detection limit
#' are coded as zeros, and detection limits provided as attributes to the data set and
#' less variables are included.
#' 
#' 
#' @name chorizonDL
#' @docType data
#' @format A data frame with 606 observations on the following 62 variables.
#' \describe{ 
#' \item{*ID }{a numeric vector} 
#' \item{XCOO }{a numeric vector} 
#' \item{YCOO }{a numeric vector} 
#' \item{Ag }{concentration in mg/kg} 
#' \item{Al }{concentration in mg/kg} 
#' \item{Al_XRF }{concentration in wt. percentage} 
#' \item{As }{concentration in mg/kg} 
#' \item{Ba }{concentration in mg/kg} 
#' \item{Ba_INAA }{concentration in mg/kg}
#' \item{Be }{concentration in mg/kg} 
#' \item{Bi }{concentration in mg/kg}
#' \item{Ca }{concentration in mg/kg} 
#' \item{Ca_XRF }{concentration in wt. percentage} 
#' \item{Cd }{concentration in mg/kg} 
#' \item{Ce_INAA }{concentration in mg/kg} 
#' \item{Co }{concentration in mg/kg} 
#' \item{Co_INAA }{concentration in mg/kg} 
#' \item{Cr }{concentration in mg/kg}
#' \item{Cr_INAA }{concentration in mg/kg}
#' \item{Cu }{concentration in mg/kg}
#' \item{Eu_INAA }{concentration in mg/kg} 
#' \item{Fe }{concentration in mg/kg} 
#' \item{Fe_XRF }{concentration in wt. percentage} 
#' \item{Hf_INAA }{concentration in mg/kg} 
#' \item{K }{concentration in mg/kg} 
#' \item{K_XRF }{concentration in wt. percentage} 
#' \item{La }{concentration in mg/kg} 
#' \item{La_INAA }{concentration in mg/kg} 
#' \item{Li }{concentration in mg/kg} 
#' \item{Lu_INAA }{concentration in mg/kg} 
#' \item{Mg }{concentration in mg/kg} 
#' \item{Mg_XRF }{concentration in wt. percentage} 
#' \item{Mn }{concentration in mg/kg} 
#' \item{Mn_XRF }{concentration in wt. percentage} 
#' \item{Na }{concentration in mg/kg} 
#' \item{Na_XRF }{concentration in wt. percentage} 
#' \item{Nd_INAA }{concentration in mg/kg} 
#' \item{Ni }{concentration in mg/kg} 
#' \item{P }{concentration in mg/kg} 
#' \item{P_XRF }{concentration in wt. percentage} 
#' \item{Pb }{concentration in mg/kg} 
#' \item{S }{concentration in mg/kg} 
#' \item{Sc }{concentration in mg/kg} 
#' \item{Sc_INAA }{concentration in mg/kg} 
#' \item{Si }{concentration in mg/kg} 
#' \item{Si_XRF }{concentration in wt. percentage} 
#' \item{Sm_INAA }{concentration in mg/kg}
#' \item{Sr }{concentration in mg/kg} 
#' \item{Th_INAA }{concentration in mg/kg} 
#' \item{Ti }{concentration in mg/kg} 
#' \item{Ti_XRF }{concentration in wt. percentage} 
#' \item{V }{concentration in mg/kg} 
#' \item{Y }{concentration in mg/kg} 
#' \item{Yb_INAA }{concentration in mg/kg} 
#' \item{Zn }{concentration in mg/kg} 
#' \item{LOI }{concentration in wt. percentage}
#' \item{pH }{ph value}
#' \item{ELEV }{elevation}
#' \item{*COUN }{country}
#' \item{*ASP }{a numeric vector} 
#' \item{TOPC }{a numeric vector}
#' \item{LITO }{information on lithography} 
#' }
#' @note For a more detailed description of this data set, see
#' \code{\link[mvoutlier]{chorizon}} in package \code{mvoutlier}.
#' @seealso \code{\link[mvoutlier]{chorizon}}, \code{\link[VIM]{chorizonDL}}
#' @references Reimann, C., Filzmoser, P., Garrett, R.G. and Dutter, R. (2008)
#' \emph{Statistical Data Analysis Explained: Applied Environmental Statistics
#' with R}. Wiley.
#' @source Kola Project (1993-1998)
#' @keywords datasets
#' @examples
#' 
#' data(chorizonDL, package = "robCompositions")
#' dim(chorizonDL)
#' colnames(chorizonDL)
#' zeroPatterns(chorizonDL)
NULL


#' coffee data set
#' 
#' 30 commercially available coffee samples of different origins.
#'
#' \itemize{
#' \item{\code{sort }}{sort of coffee}
#' \item{\code{acit }}{acetic acid }
#' \item{\code{metpyr }}{methylpyrazine}
#' \item{\code{furfu }}{furfural}
#' \item{\code{furfualc }}{furfuryl alcohol}
#' \item{\code{dimeth }}{2,6 dimethylpyrazine}
#' \item{\code{met5 }}{5-methylfurfural}
#' }
#'
#' @name coffee
#' @docType data
#' @usage data(coffee)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}, Karel Hron
#' @details In the original data set, 15 volatile compounds (descriptors of coffee aroma) were selected for a statistical analysis. We selected six compounds (compositional parts) on three sorts of coffee.
#' @format A data frame with 30 observations and 7 variables.
#' @references M. Korhonov\'a, K. Hron, D. Klimc\'ikov\'a, L. Muller, P. Bedn\'ar, and P. Bart\'ak (2009). Coffee aroma - statistical analysis of compositional data. \emph{Talanta}, 80(2): 710--715.
#' @keywords data
#' @examples 
#' 
#' data(coffee)
#' str(coffee)
#' summary(coffee)
NULL

#' economic indicators
#' 
#' Household and government consumptions, gross captial formation and import and exports of goods 
#' and services.
#'
#' \itemize{
#'  \item{\code{country }}{country name}
#'  \item{\code{country2 }}{country name, short version}
#'  \item{\code{HHconsumption }}{Household and NPISH final consumption expenditure}
#'  \item{\code{GOVconsumption }}{Final consumption expenditure of general government}
#'  \item{\code{capital }}{Gross capital formation}
#'  \item{\code{exports }}{Exports of goods and services}
#'  \item{\code{imports }}{Imports of goods and services}
#' }
#'
#' @name economy
#' @docType data
#' @usage data(economy)
#' @author Peter Filzmoser, Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A data frame with 30 observations and 7 variables
#' @references Eurostat, \url{http://ec.europa.eu/eurostat/data}
#' @keywords data
#' @examples 
#' 
#' data(economy)
#' str(economy)
NULL

#' education level of father (F) and mother (M) 
#' 
#' Education level of father (F) and mother (M) in percentages of low
#' (l), medium (m), and high (h) of 31 countries in Europe.
#' 
#' \itemize{
#'  \item{\code{country }}{community code}
#'  \item{\code{F.l }}{percentage of females with low edcuation level}
#'  \item{\code{F.m }}{percentage of females with medium edcuation level}
#'  \item{\code{F.h }}{percentage of females with high edcuation level}
#'  \item{\code{F.l }}{percentage of males with low edcuation level}
#'  \item{\code{F.m }}{percentage of males with medium edcuation level}
#'  \item{\code{F.h }}{percentage of males with high edcuation level}
#' }
#'
#' @name educFM
#' @docType data
#' @usage data(educFM)
#' @author Peter Filzmoser, Matthias Templ
#' @format A data frame with 31 observations and 8 variables
#' @keywords data
#' @source from Eurostat,\url{http://ec.europa.eu/eurostat/}
#' @examples 
#' 
#' data(educFM)
#' str(educFM)
NULL

#' election data
#' 
#' Results of a election in Germany 2013 in different
#' federal states 
#' 
#' Votes for the political parties
#' in the elections (compositional variables), and their relation to the unemployment rate
#' and the average monthly income (external non-compositional variables). Votes are for the Christian Democratic Union and Christian Social Union of Bavaria, also
#' called The Union (CDU/CSU), Social Democratic Party (SDP), The Left (DIE LINKE),
#' Alliance '90/The Greens (GRUNE), Free Democratic Party (FDP) and the rest of the
#' parties participated in the elections (other parties). The votes are examined in absolute
#' values (number of valid votes). The unemployment in the federal states is reported in
#' percentages, and the average monthly income in Euros.
#'
#' \itemize{
#'  \item{\code{CDU_CSU }}{Christian Democratic Union and Christian Social Union of Bavaria, also
#' called The Union}
#'  \item{\code{SDP }}{Social Democratic Party}
#'  \item{\code{GRUENE }}{Alliance '90/The Greens}
#'  \item{\code{FDP }}{Free Democratic Party}
#'  \item{\code{DIE_LINKE }}{The Left}
#'  \item{\code{other_parties }}{Votes for the rest of the
#' parties participated in the elections}
#'  \item{\code{unemployment }}{Unemployment in the federal states in percentages}
#'  \item{\code{income }}{Average monthly income in Euros}
#' }
#'
#' @name election
#' @docType data
#' @usage data(election)
#' @author Petra Klynclova, Matthias Templ
#' @format A data frame with 16 observations and 8 variables
#' @references Eurostat, \url{http://ec.europa.eu/eurostat/data}
#' @keywords data
#' @source German Federal Statistical Office 
#' @examples 
#' 
#' data(election)
#' str(election)
NULL

#' Austrian presidential election data
#' 
#' Results the Austrian presidential election in October 2016.
#' 
#' Votes for the candidates Hofer and Van der Bellen.
#'
#' \itemize{
#'  \item{\code{GKZ }}{Community code}
#'  \item{\code{Name }}{Name of the community}
#'  \item{\code{Eligible }}{eligible votes}
#'  \item{\code{Votes_total }}{total votes}
#'  \item{\code{Votes_invalid }}{invalid votes}
#'  \item{\code{Votes_valid }}{valid votes}
#'  \item{\code{Hofer_total }}{votes for Hofer}
#'  \item{\code{Hofer_perc }}{votes for Hofer in percentages}
#'  \item{\code{VanderBellen_total }}{votes for Van der Bellen}
#'  \item{\code{VanderBellen_perc }}{votes for Van der Bellen in percentages}
#' }
#'
#' @name electionATbp
#' @docType data
#' @usage data(electionATbp)
#' @author Peter Filzmoser
#' @format A data frame with 2202 observations and 10 variables
#' @keywords data
#' @source OpenData Austria, \url{https://www.data.gv.at/}
#' @examples 
#' 
#' data(electionATbp)
#' str(electionATbp)
NULL


#' employment in different countries by gender and status.
#'
#' @name employment
#' @docType data
#' @usage data(employment)
#' @author Valentin Todorov
#' @format A three-dimensional table
#' @keywords data
#' @examples 
#' data(employment)
#' str(employment)
#' employment
NULL

#' Employment in different countries by gender and status.
#'
#' \itemize{
#'  \item{\code{gender}}{factor}
#'  \item{\code{status}}{factor, defining if part or full time work}
#'  \item{\code{country}}{country}
#'  \item{\code{value}}{employment}
#' }
#'
#' @name employment_df
#' @docType data
#' @usage data(employment_df)
#' @author Valentin Todorov, Matthias Templ
#' @format A data.frame with 132 rows and 4 columns. 
#' @keywords data
#' @examples 
#' data(employment_df)
#' head(employment_df)
NULL


#' synthetic household expenditures toy data set
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


#' mean consumption expenditures data.
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
#' @source Eurostat
#' @keywords datasets
#' @examples
#' 
#' data(expendituresEU)
#' 
NULL


#' GDP satisfaction 
#' 
#' Satisfaction of GDP in 31 countries. The GDP is measured per capita from the year 2012.
#'
#' \itemize{
#'  \item{\code{country }}{community code}
#'  \item{\code{gdp }}{GDP per capita in 2012}
#'  \item{\code{very.bad }}{satisfaction very bad}
#'  \item{\code{bad }}{satisfaction bad}
#'  \item{\code{moderately.bad }}{satisfaction moderately bad}
#'  \item{\code{moderately.good }}{satisfaction moderately good}
#'  \item{\code{good }}{satisfaction good}
#'  \item{\code{very.good }}{satisfaction very good}
#' }
#'
#' @name GDPsatis
#' @docType data
#' @usage data(GDPsatis)
#' @author Peter Filzmoser, Matthias Templ
#' @format A data frame with 31 observations and 8 variables
#' @keywords data
#' @source from Eurostat,\url{http://ec.europa.eu/eurostat/}
#' @examples 
#' 
#' data(GDPsatis)
#' str(GDPsatis)
NULL

#' GEMAS geochemical data set
#'
#' Geochemical data set on agricultural and grazing land soil
#'
#' \itemize{
#'  \item{\code{COUNTRY }}{country name}
#'  \item{\code{longitude }}{longitude in WGS84}
#'  \item{\code{latitude }}{latitude in WGS84}
#'  \item{\code{Xcoord }}{UTM zone east}
#'  \item{\code{Ycoord }}{UTM zone north}
#'  \item{\code{MeanTemp}}{Annual mean temperature}
#'  \item{\code{AnnPrec }}{Annual mean precipitation}
#'  \item{\code{soilclass }}{soil class}
#'  \item{\code{sand }}{sand}
#'  \item{\code{silt }}{silt}
#'  \item{\code{clay }}{clay}
#'  \item{\code{Al }}{Concentration of aluminum (in mg/kg)}
#'  \item{\code{Ba }}{Concentration of barium (in mg/kg)}
#'  \item{\code{Ca }}{Concentration of calzium (in mg/kg)}\
#'  \item{\code{Cr }}{Concentration of chromium (in mg/kg)}
#'  \item{\code{Fe }}{Concentration of iron (in mg/kg)}
#'  \item{\code{K }}{Concentration of pottasium (in mg/kg)}
#'  \item{\code{Mg }}{Concentration of magnesium (in mg/kg)}
#'  \item{\code{Mn }}{Concentration of manganese (in mg/kg)}
#'  \item{\code{Na }}{Concentration of sodium (in mg/kg)}
#'  \item{\code{Nb }}{Concentration of niobium (in mg/kg)}
#'  \item{\code{Ni }}{Concentration of nickel (in mg/kg)}
#'  \item{\code{P }}{Concentration of phosphorus (in mg/kg)}
#'  \item{\code{Si }}{Concentration of silicium (in mg/kg)}
#'  \item{\code{Sr }}{Concentration of strontium (in mg/kg)}
#'  \item{\code{Ti }}{Concentration of titanium (in mg/kg)}
#'  \item{\code{V }}{Concentration of vanadium (in mg/kg)}\
#'  \item{\code{Y }}{Concentration of yttrium (in mg/kg)}
#'  \item{\code{Zn }}{Concentration of zinc (in mg/kg)}
#'  \item{\code{Zr }}{Concentration of zirconium (in mg/kg)}
#'  \item{\code{LOI }}{Loss on ignition (in wt-percent)}
#' }
#'
#' @name gemas
#' @docType data
#' @usage data(gemas)
#' @author GEMAS is a cooperation project between the EuroGeoSurveys Geochemistry Expert Group and Eurometaux. Integration in R, Peter Filzmoser and Matthias Templ.
#' @format A data frame with 2108 observations and 30 variables
#' @references Reimann, C., Birke, M., Demetriades, A., Filzmoser, P. \& O'Connor, P. (Editors), 2014. Chemistry of Europe's agricultural soils - Part A: Methodology and interpretation of the GEMAS data set. Geologisches Jahrbuch (Reihe B 102), Schweizerbarth, Hannover, 528 pp. + DVD 
#' Reimann, C., Birke, M., Demetriades, A., Filzmoser, P. & O'Connor, P. (Editors), 2014. Chemistry of Europe's agricultural soils - Part B: General background information and further analysis of the GEMAS data set. Geologisches Jahrbuch (Reihe B 103), Schweizerbarth, Hannover, 352 pp. 
#' @details The sampling, at a density of 1 site/2500 sq. km, was completed at the beginning of 2009 by collecting 2211 samples of agricultural soil (Ap-horizon, 0-20 cm, regularly ploughed fields), and 2118 samples from land under permanent grass cover (grazing land soil, 0-10 cm), according to an agreed field protocol.
#' All GEMAS project samples were shipped to Slovakia for sample preparation, where they were air dried, sieved to <2 mm using a nylon screen, homogenised and split to subsamples for analysis. They were analysed for a large number of chemical elements. In this sample, the main elements by X-ray fluorescence are included as well as the composition on sand, silt, clay.
#' @keywords data
#' @examples 
#' 
#' data(gemas)
#' str(gemas)
#' ## sample sites
#' \dontrun{
#' require(ggmap)
#' map <- get_map("europe", source = "stamen", maptype = "watercolor", zoom=4)
#' ggmap(map) + geom_point(aes(x=longitude, y=latitude), data=gemas)
#' map <- get_map("europe", zoom=4)
#' ggmap(map) + geom_point(aes(x=longitude, y=latitude), data=gemas, size=0.8)
#' }
NULL




#' government spending
#' 
#' Government expenditures based on COFOG categories
#' 
#' The general government sector consists of central, 
#' state and local governments, and the social security funds controlled 
#' by these units. The data are based on the system of national accounts, 
#' a set of internationally agreed concepts, definitions, classifications 
#' and rules for national accounting. The classification of functions of government 
#' (COFOG) is used as classification system. 
#' The central government spending by category is measured as a percentage 
#' of total expenditures.
#' 
#' @name govexp
#' @docType data
#' @format A (tidy) data frame with 5140 observations on the following 4 variables.
#' \itemize{ 
#' \item{\code{country }}{Country of origin}
#' \item{\code{category }}{The COFOG expenditures are divided into 
#' in the following ten categories: general public services; 
#' defence; public order and safety; economic affairs; 
#' environmental protection; housing and community amenities; 
#' health; recreation, culture and religion; education; and 
#' social protection. } 
#' \item{\code{year }}{Year} 
#' \item{\code{value }}{COFOG spendings/expenditures}
#' }
#' @author translated from \url{https://data.oecd.org/} and restructured by Matthias Templ
#' @source OECD:
#' \url{https://data.oecd.org/}
#' @keywords datasets
#' @examples
#' 
#' data(govexp)
#' str(govexp)
NULL


#' haplogroups data.
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

#' value added, output and input for different ISIC codes and countries.
#'
#' \itemize{
#'  \item{\code{ct}}{ct}
#'  \item{\code{isic}}{ISIC classification, Rev 3.2}
#'  \item{\code{VA}}{value added}
#'  \item{\code{OUT}}{output}
#'  \item{\code{INP}}{input}
#'  \item{\code{IS03}}{country code}
#'  \item{\code{mht}}{mht}
#' }
#'
#' @name instw
#' @docType data
#' @usage data(instw)
#' @author Valentin Todorov, Matthias Templ
#' @format A data.frame with 1555 rows and 7 columns. 
#' @keywords data
#' @examples 
#' data(instw)
#' head(instw)
NULL

#' ISIC codes by name
#'
#' \itemize{
#'  \item{\code{code}}{ISIC code, Rev 3.2}
#'  \item{\code{description}}{Description of ISIC codes}
#' }
#'
#' @name isic32
#' @docType data
#' @usage data(isic32)
#' @author Valentin Todorov, Matthias Templ
#' @format A data.frame with 24 rows and 2 columns. 
#' @keywords data
#' @examples 
#' data(instw)
#' instw
NULL



#' labour force by status in employment 
#' 
#' Labour force by status in employment for 124 countries, latest update: December 2009
#' 
#' \itemize{
#' \item{\code{country }}{country}                            
#' \item{\code{year }}{year}                                      
#' \item{\code{employeesW }}{percentage female employees}                             
#' \item{\code{employeesM }}{percentage male employees}
#' \item{\code{employersW }}{percentage female employers}                    
#' \item{\code{employersM }}{percentage male employers}
#' \item{\code{ownW }}{percentage female own-account workers and contributing family workers}               
#' \item{\code{ownM }}{percentage male own-account workers and contributing family workers} 
#' \item{\code{source }}{HS: household or labour force survey. OE: official estimates. PC: population census} 
#' }
#' 
#' @name laborForce
#' @docType data
#' @format A data set on 124 compositions on 9 variables.
#' @author conversion to R by Karel Hron and Matthias Templ \email{matthias.templ@@tuwien.ac.at} 
#' @source
#' \url{http://unstats.un.org/unsd/demographic/products/indwm/tab5c.htm}
#' @keywords datasets
#' @references K. Hron, P. Filzmoser, K. Thompson (2012). Linear regression with compositional explanatory variables. \emph{Journal of Applied Statistics}, Volume 39, Issue 5, 2012. 
#' @examples
#' 
#' data(laborForce)
#' str(laborForce)
#' 
NULL



#' life expectancy and GDP (2008) for EU-countries
#' 
#' Social-economic data for compositional regression.
#' 
#' \itemize{
#' \item{\code{country }}{country}                                   
#' \item{\code{agriculture }}{GDP on agriculture, hunting, forestry, fishing (ISIC A-B, x1)}               
#' \item{\code{manufacture }}{GDP on mining, manufacturing, utilities (ISIC C-E, x2)}
#' \item{\code{construction }}{GDP on construction (ISIC F, x3)}              
#' \item{\code{wholesales }}{GDP on wholesale, retail trade, restaurants and hotels (ISIC G-H, x4)}
#' \item{\code{transport }}{GDP on transport, storage and communication (ISIC I, x5)}
#' \item{\code{other }}{GDP on other activities (ISIC J-P, x6)}
#' \item{\code{lifeExpMen }}{life expectancy for men and women}
#' \item{\code{lifeExpWomen }}{life expectancy for men and women}
#' }
#' 
#' @name lifeExpGdp
#' @docType data
#' @format A data set on 27 compositions on 9 variables.
#' @source
#' \url{http://www.ec.europa.eu/eurostat} and \url{http://unstats.un.org/unsd}
#' @keywords datasets
#' @author conversion to R by Karel Hron and Matthias Templ \email{matthias.templ@@tuwien.ac.at} 
#' @references K. Hron, P. Filzmoser, K. Thompson (2012). Linear regression with compositional explanatory variables. \emph{Journal of Applied Statistics}, Volume 39, Issue 5, 2012. 
#' @examples
#' 
#' data(lifeExpGdp)
#' str(lifeExpGdp)
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


#' metabolomics mcad data set
#' 
#' The aim of the experiment was to ascertain novel biomarkers of 
#' MCAD (Medium chain acyl-CoA dehydrogenase) deficiency. 
#' The data consists of 25 patients and 25 controls and the analysis was done by LC-MS.
#' Rows represent patients and controls and columns represent chemical 
#' entities with their quantity.  
#'
#' \itemize{
#'  \item{\code{group }}{patient group}
#'  \item{\code{... }}{the remaining variables columns are represented by m/z which are chemical characterizations of individual chemical components on exact mass measurements..}
#' }
#'
#' @name mcad
#' @docType data
#' @usage data(mcad)
#' @format A data frame with 50 observations and 279 variables
#' @keywords data
#' @references Najdekr L., Gardlo A., Madrova L., Friedeckyy D., Janeckova H., Correa E.S., Goodacre R., Adam T., Oxidized phosphatidylcholines suggest oxidative stress in patients with medium-chain acyl-CoA dehydrogenase deficiency, \emph{Talanta} 139, 2015, 62-66.
#' @examples 
#' 
#' data(mcad)
#' str(mcad)
NULL


#' mortality and life expectancy in the EU
#' 
#'
#' \itemize{
#'  \item{\code{country }}{country name}
#'  \item{\code{country2 }}{country name, short version}
#'  \item{\code{sex }}{gender}
#'  \item{\code{lifeExpectancy }}{life expectancy}
#'  \item{\code{infectious }}{certain infectious and parasitic diseases (A00-B99)}
#'  \item{\code{neoplasms }}{malignant neoplasms (C00-C97)}
#'  \item{\code{endocrine }}{endocrine nutritional and metabolic diseases (E00-E90)}
#'  \item{\code{mental }}{mental and behavioural disorders (F00-F99)}
#'  \item{\code{nervous }}{diseases of the nervous system and the sense organs (G00-H95)}
#'  \item{\code{circulatory }}{diseases of the circulatory system (I00-I99)}
#'  \item{\code{respiratory }}{diseases of the respiratory system (J00-J99)}
#'  \item{\code{digestive }}{diseases of the digestive system (K00-K93)}
#' }
#'
#' @name mortality
#' @docType data
#' @usage data(mortality)
#' @author Peter Filzmoser, Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A data frame with 60 observations and 12 variables
#' @references Eurostat, \url{http://ec.europa.eu/eurostat/data}
#' @keywords data
#' @examples 
#' 
#' data(mortality)
#' str(mortality)
#' ## totals (mortality)
#' aggregate(mortality[,5:ncol(mortality)], 
#'           list(mortality$country2), sum)
NULL

#' mortality table 
#' 
#' Mortality data by gender, unknown year
#'
#' \itemize{
#'  \item{\code{female}}{mortality rates for females by age groups}
#'  \item{\code{male}}{mortality rates for males by age groups}
#' }
#'
#' @name mortality_tab
#' @docType data
#' @usage data(mortality_tab)
#' @author Matthias Templ
#' @format A table
#' @keywords data
#' @examples 
#' data(mortality_tab)
#' mortality_tab
NULL


#' nutrient contents
#' 
#' Nutrients on more than 40 components and 965 generic food products
#'
#' \itemize{
#' \item{\code{ID }}{ID, for internal use}
#' \item{\code{ID_V4 }}{ID V4, for internal use}
#' \item{\code{ID_SwissFIR }}{ID, for internal use}
#' \item{\code{name_D }}{Name in German}
#' \item{\code{name_F }}{Name in French}
#' \item{\code{name_I }}{Name in Italian}
#' \item{\code{name_E }}{Name in Spanish}
#' \item{\code{category_D }}{Category name in German}
#' \item{\code{category_F }}{Category name in French}
#' \item{\code{category_I }}{Category name in Italy}
#' \item{\code{category_E }}{Category name in Spanish}
#' \item{\code{gravity }}{specific gravity}
#' \item{\samp{energy_kJ }}{energy in kJ per 100g edible portion}
#' \item{\code{energy_kcal }}{energy in kcal per 100g edible portion}
#' \item{\code{protein }}{protein in gram per 100g edible portion}
#' \item{\code{alcohol }}{alcohol in gram per 100g edible portion}
#' \item{\code{water }}{water in gram per 100g edible portion}
#' \item{\code{carbohydrates}}{crbohydrates in gram per 100g edible portion}
#' \item{\code{starch }}{starch in gram per 100g edible portion}
#' \item{\code{sugars }}{sugars in gram per 100g edible portion}
#' \item{\samp{dietar_ fibres }}{dietar fibres in gram per 100g edible portion}
#' \item{\code{fat }}{fat in gram per 100g edible portion}
#' \item{\code{cholesterol }}{cholesterolin milligram per 100g edible portion}
#' \item{\code{fattyacids_monounsaturated }}{fatty acids monounsatrurated in gram per 100g edible portion}
#' \item{\code{fattyacids_saturated }}{fatty acids saturated in gram per 100g edible portion}
#' \item{\code{fatty_acids_polyunsaturated }}{fatty acids polyunsaturated in gram per 100g edible portion}
#' \item{\code{vitaminA }}{vitamin A in retinol equivalent per 100g edible portion}
#' \item{\samp{all-trans_retinol_equivalents }}{all trans-retinol equivalents in gram per 100g edible portion}
#' \item{\samp{beta-carotene-activity }}{beta-carotene activity in beta-carotene equivalent per 100g edible portion}
#' \item{\samp{beta-carotene }}{beta-carotene in micogram per 100g edible portion}
#' \item{\code{vitaminB1 }}{vitamin B1 in milligram per 100g edible portion}
#' \item{\code{vitaminB2 }}{vitamin B2 in milligram per 100g edible portion}
#' \item{\code{vitaminB6 }}{vitamin B6 in milligram per 100g edible portion}
#' \item{\code{vitaminB12 }}{vitamin B12 in micogram per 100g edible portion}
#' \item{\code{niacin }}{niacin in milligram per 100g edible portion}
#' \item{\code{folate }}{folate in micogram per 100g edible portion}
#' \item{\code{pantothenic_acid }}{pantothenic acid in milligram per 100g edible portion}
#' \item{\code{vitaminC }}{vitamin C in milligram per 100g edible portion}
#' \item{\code{vitaminD }}{vitamin D in micogram per 100g edible portion}
#' \item{\code{vitaminE }}{vitamin E in alpha-tocopherol equivalent per 100g edible portion}
#' \item{\code{Na }}{Sodium in milligram per 100g edible portion}
#' \item{\code{K }}{Potassium in milligram per 100g edible portion}
#' \item{\code{Cl }}{Chloride}
#' \item{\code{Ca }}{Calcium}
#' \item{\code{Mg }}{Magnesium}
#' \item{\code{P }}{Phosphorus}
#' \item{\code{Fe }}{Iron}
#' \item{\code{I }}{Iodide in milligram per 100g edible portion}
#' \item{\code{Zn }}{Zink}
#' \item{\code{unit }}{a factor with levels \code{per 100g edible portion} \code{per 100ml food volume}}
#' }
#'
#' @name nutrients
#' @docType data
#' @usage data(nutrients)
#' @author Translated from the Swiss nutrion data base by Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @source From the Swiss nutrition data base 2015 (second edition), see \url{ http://www.sge-ssn.ch/shop/produkt/schweizer-naehrwerttabelle/}
#' @format A data frame with 965 observations on the following 50 variables.
#' @references \url{ http://www.sge-ssn.ch/shop/produkt/schweizer-naehrwerttabelle/}
#' @keywords data
#' @examples 
#' 
#' data(nutrients)
#' str(nutrients)
#' head(nutrients[, 41:49])
NULL

#' nutrient contents (branded)
#' 
#' Nutrients on more than 10 components and 9618 branded food products
#'
#' \itemize{
#' \item{\code{name_D }}{name (in German)}
#' \item{\code{category_D }}{factor specifying the category names}
#' \item{\code{category_F }}{factor specifying the category names}
#' \item{\code{category_I }}{factor specifying the category names}
#' \item{\code{category_E }}{factor specifying the category names}
#' \item{\code{gravity }}{specific gravity}
#' \item{\code{energy_kJ }}{energy in kJ}
#' \item{\samp{energy_kcal }}{energy in kcal}
#' \item{\code{protein }}{protein in gram}
#' \item{\code{alcohol }}{alcohol in gram}
#' \item{\code{water }}{water in gram}
#' \item{\code{carbohydrates_available }}{available carbohydrates in gram}
#' \item{\code{sugars }}{sugars in gram}
#' \item{\code{dietary_fibres }}{dietary fibres in gram}
#' \item{\code{fat_total }}{total fat in gram}
#' \item{\code{fatty_acids_saturated }}{saturated acids fat in gram}
#' \item{\code{Na }}{Sodium in gram}
#' \item{\code{unit }}{a factor with levels \code{per 100g edible portion} \code{per 100ml food volume}}
#' }
#'
#' @name nutrients_branded
#' @docType data
#' @usage data(nutrients_branded)
#' @author Translated from the Swiss nutrion data base by Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @source From the Swiss nutrition data base 2015 (second edition), see \url{ http://www.sge-ssn.ch/shop/produkt/schweizer-naehrwerttabelle/}
#' @format A data frame with 9618 observations on the following 18 variables.
#' @references \url{ http://www.sge-ssn.ch/shop/produkt/schweizer-naehrwerttabelle/}
#' @keywords data
#' @examples 
#' 
#' data(nutrients_branded)
#' str(nutrients_branded)
NULL


#' special payments
#' 
#' Payments splitted by different NACE categories and kind of employment in Austria 2004
#'
#' \itemize{
#'  \item{\code{nace }}{NACE classification, 2 digits}
#'  \item{\code{oenace_2008 }}{Corresponding Austrian NACE classification (in German)}
#'  \item{\code{year }}{year}
#'  \item{\code{month }}{month}
#'  \item{\code{localunit }}{local unit ID}
#'  \item{\code{spay }}{special payments (total)}
#'  \item{\code{spay_wc }}{special payments for white colar workers}
#'  \item{\code{spay_bc }}{special payments for blue colar workers}
#'  \item{\code{spay_traintrade }}{special payments for trainees in trade businness}
#'  \item{\code{spay_home }}{special payments for home workers}
#'  \item{\code{spay_traincomm }}{special payments for trainees in commercial businness}
#' }
#'
#' @name payments
#' @docType data
#' @usage data(payments)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @source statCube data base at \url{www.statistik.ac.at}. The product and all 
#' material contained therein are protected by copyright with all rights 
#' reserved by the Bundesanstalt Statistik Oesterreich (STATISTICS AUSTRIA). 
#' It is permitted to reproduce, distribute, make publicly available 
#' and process the content for non-commercial purposes. Prior to any use for 
#' commercial purposes a written consent of STATISTICS AUSTRIA must be obtained. 
#' Any use of the contained material must 
#' be correctly reproduced and clearly cite the source STATISTICS AUSTRIA. 
#' If tables published by STATISTICS AUSTRIA are partially used, displayed or 
#' otherwise changed, a note must be added at an adequate position to 
#' show data was extracted or adapted. 
#' @format A data frame with 535 rows and 11 variables
#' @keywords data
#' @examples 
#' 
#' data(payments)
#' str(payments)
#' summary(payments)
NULL


#' PhD students in the EU
#' 
#' PhD students in Europe based on the standard classification system splitted
#' by different kind of studies (given as percentages).
#' 
#' Due to unknown reasons the rowSums of the percentages is not always 100.
#' 
#' \itemize{
#' \item{\code{country }}{country of origin (German)}  
#' \item{\code{countryEN }}{country of origin (English)}    
#' \item{\code{country2 }}{country of origin, 2-digits}   
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
#' @format A data set on 32 compositions and 11 variables.
#' @source Eurostat
#' @references Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods. \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages 3095-3107.
#' @keywords datasets
#' @examples
#' 
#' data(phd)
#' str(phd)
#' 
NULL

#' PhD students in the EU (totals)
#' 
#' PhD students in Europe by different kind of studies.
#' 
#' \itemize{
#' \item{\code{technical }}{phd students in natural and technical sciences}
#' \item{\code{socio-economic-low }}{phd students in social sciences, economic sciences and law sciences}                    
#' \item{\code{human }}{phd students in human sciences including teaching}
#' \item{\code{health }}{phd students in health and life sciences}               
#' \item{\code{agriculture }}{phd students in agriculture} 
#' }
#' 
#' @name phd_totals
#' @docType data
#' @format A data set on 29 compositions and 5 variables.
#' @source Eurostat
#' @references Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods. \emph{Computational Statistics and Data Analysis}, vol 54 (12), pages 3095-3107.
#' @keywords datasets
#' @examples
#' 
#' data("phd_totals")
#' str(phd_totals)
#' 
NULL

#' 24-hour precipitation
#' 
#' table containing counts for 24-hour precipitation for season at the rain-gouge.
#'
#' \itemize{
#'  \item{\code{spring}}{numeric vector on counts for different level of precipitation}
#'  \item{\code{summer}}{numeric vector on counts for different level of precipitation}
#'  \item{\code{autumn}}{numeric vector on counts for different level of precipitation}
#'  \item{\code{winter}}{numeric vector on counts for different level of precipitation}
#' }
#'
#' @name precipitation
#' @docType data
#' @usage data(precipitation)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A table with 4 rows and 6 columns
#' @references Romero R, Guijarro J A, Ramis C, Alonso S (1998). A 30-years (196493) daily rainfall
#' data base for the Spanish Mediterranean regions: first exploratory study. 
#' \emph{International Journal of Climatology} 18, 541560.
#' @keywords data
#' @examples 
#' data(precipitation)
#' precipitation
#' str(precipitation)
NULL

#' production splitted by nationality on enterprise level
#'
#' \itemize{
#'  \item{\code{nace }}{NACE classification, 2 digits}
#'  \item{\code{oenace_2008 }}{Corresponding Austrian NACE classification (in German)}
#'  \item{\code{year }}{year}
#'  \item{\code{month }}{month}
#'  \item{\code{enterprise }}{enterprise ID}
#'  \item{\code{total }}{total ...}
#'  \item{\code{home }}{home ...}
#'  \item{\code{EU }}{EU ...}
#'  \item{\code{non-EU }}{non-EU ...}
#' }
#'
#' @name production
#' @docType data
#' @usage data(production)
#' @author Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @source statCube data base at \url{www.statistik.ac.at}. The product and all 
#' material contained therein are protected by copyright with all rights 
#' reserved by the Bundesanstalt Statistik Oesterreich (STATISTICS AUSTRIA). 
#' It is permitted to reproduce, distribute, make publicly available 
#' and process the content for non-commercial purposes. Prior to any use for 
#' commercial purposes a written consent of STATISTICS AUSTRIA must be obtained. 
#' Any use of the contained material must 
#' be correctly reproduced and clearly cite the source STATISTICS AUSTRIA. 
#' If tables published by STATISTICS AUSTRIA are partially used, displayed or 
#' otherwise changed, a note must be added at an adequate position to 
#' show data was extracted or adapted. 
#' @format A data frame with 535 rows and 9 variables
#' @keywords data
#' @examples 
#' 
#' data(production)
#' str(production)
#' summary(production)
NULL

#' codes for UNIDO tables
#'
#' \itemize{
#'  \item{\code{ISOCN}}{ISOCN codes}
#'  \item{\code{OPERATOR}}{Operator}
#'  \item{\code{ADESC}}{Country}
#'  \item{\code{CCODE}}{Country code}
#'  \item{\code{CDESC}}{Country destination}
#'  \item{\code{ACODE}}{Country destination code}
#' }
#'
#' @name rcodes
#' @docType data
#' @usage data(rcodes)
#' @author Valentin Todorov
#' @format A data.frame with 2717 rows and 6 columns.  
#' @keywords data
#' @examples 
#' data(rcodes)
#' str(rcodes)
NULL

#' aphyric skye lavas data 
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


#' social expenditures
#' 
#' Social expenditures according to source (public or private) 
#' and three important branches (health, old age, incapacity related) in 
#' selected OECD countries in 2010. Expenditures are always provided 
#' in the respective currency.
#'
#' \itemize{
#' \item{\code{country }}{Country of origin}
#' \item{\code{currency }}{Currency unit (in Million)}
#' \item{\code{health-public }}{Health from the public}
#' \item{\code{old-public }}{Old age expenditures from the public}
#' \item{\code{incap-public }}{Incapacity related expenditures from the public}
#' \item{\code{health-private }}{Health from private sources}
#' \item{\code{old-private }}{Old age expenditures from private sources}
#' \item{\code{incap-private }}{Incapacity related expenditures from private sources}
#' }
#'
#' @name socExp
#' @docType data
#' @usage data(socExp)
#' @author conversion to R by Karel Hron Karel Hron and modifications by Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A data frame with 20 observations on the following 8 variables (country + currency + row-wise sorted cells of 2x3 compositional table).
#' @references OECD, \url{http://www.oecd.org}
#' @keywords data
#' @examples 
#' 
#' data(socExp)
#' str(socExp)
#' rowSums(socExp[, 3:ncol(socExp)])
NULL


#' teaching stuff
#' 
#' Teaching stuff in selected countries
#' 
#' Teaching staff include professional personnel directly 
#' involved in teaching students, including classroom 
#' teachers, special education teachers and other 
#' teachers who work with students as a whole class, 
#' in small groups, or in one-to-one teaching. 
#' Teaching staff also include department chairs 
#' of whose duties include some teaching, but 
#' it does not include non-professional personnel 
#' who support teachers in providing instruction 
#' to students, such as teachers' aides and other 
#' paraprofessional personnel. Academic staff include 
#' personnel whose primary assignment is instruction, 
#' research or public service, holding an academic 
#' rank with such titles as professor, associate 
#' professor, assistant professor, instructor, 
#' lecturer, or the equivalent of any of these 
#' academic ranks. The category includes personnel 
#' with other titles (e.g. dean, director, associate 
#' dean, assistant dean, chair or head of department), 
#' if their principal activity is instruction or research.
#' 
#' @name teachingStuff
#' @docType data
#' @format A (tidy) data frame with 1216 observations on the following 4 variables.
#' \itemize{ 
#' \item{\code{country }}{Country of origin}
#' \item{\code{subject }}{school type: primary, lower secondary, higher secondary and tertiary } 
#' \item{\code{year }}{Year} 
#' \item{\code{value }}{Number of stuff}
#' }
#' @author translated from \url{https://data.oecd.org/} and restructured by Matthias Templ
#' @references OECD (2017), Teaching staff (indicator). doi: 10.1787/6a32426b-en (Accessed on 27 March 2017)
#' @source OECD:
#' \url{https://data.oecd.org/}
#' @keywords datasets
#' @examples
#' 
#' data(teachingStuff)
#' str(teachingStuff)
NULL


#' regional geochemical survey of soil C in Norway
#' 
#' A regional-scale geochemical survey of C horizon samples in Nord-Trondelag, Central Norway
#'
#' \itemize{
#'  \item{\code{X.S_ID }}{ID}
#'  \item{\code{X.Loc_ID }}{ID}
#'  \item{\code{longitude }}{longitude in WGS84}
#'  \item{\code{latitude }}{latitude in WGS84}
#'  \item{\code{E32wgs }}{UTM zone east}
#'  \item{\code{N32wgs }}{UTM zone north}
#'  \item{\code{X.Medium }}{}
#'  \item{\code{Ag }}{Concentration of silver (in mg/kg)}
#'  \item{\code{Al }}{Concentration of aluminum (in mg/kg)}
#'  \item{\code{As }}{Concentration of arsenic (in mg/kg)}
#'  \item{\code{Au }}{Concentration of gold (in mg/kg)}
#'  \item{\code{B }}{Concentration of boron (in mg/kg)}
#'  \item{\code{Ba }}{Concentration of barium (in mg/kg)}
#'  \item{\code{Be }}{Concentration of beryllium (in mg/kg)}
#'  \item{\code{Bi }}{Concentration of bismuth (in mg/kg)}
#'  \item{\code{Ca }}{Concentration of calzium (in mg/kg)}
#'  \item{\code{Cd }}{Concentration of cadmium (in mg/kg)}
#'  \item{\code{Ce }}{Concentration of cerium (in mg/kg)}
#'  \item{\code{Co }}{Concentration of cobalt (in mg/kg)}
#'  \item{\code{Cr }}{Concentration of chromium (in mg/kg)}
#'  \item{\code{Cs }}{Concentration of cesium (in mg/kg)}
#'  \item{\code{Cu }}{Concentration of copper (in mg/kg)}
#'  \item{\code{Fe }}{Concentration of iron (in mg/kg)}
#'  \item{\code{Ga }}{Concentration of gallium (in mg/kg)}
#'  \item{\code{Ge }}{Concentration of germanium (in mg/kg)}
#'  \item{\code{Hf }}{Concentration of hafnium (in mg/kg)}
#'  \item{\code{Hg }}{Concentration of mercury (in mg/kg)}
#'  \item{\code{In }}{Concentration of indium (in mg/kg)}
#'  \item{\code{K }}{Concentration of pottasium (in mg/kg)}
#'  \item{\code{La }}{Concentration of lanthanum (in mg/kg)}
#'  \item{\code{Li }}{Concentration of lithium (in mg/kg)}
#'  \item{\code{Mg }}{Concentration of magnesium (in mg/kg)}
#'  \item{\code{Mn }}{Concentration of manganese (in mg/kg)}
#'  \item{\code{Mo }}{Concentration of molybdenum (in mg/kg)}
#'  \item{\code{Na }}{Concentration of sodium (in mg/kg)}
#'  \item{\code{Nb }}{Concentration of niobium (in mg/kg)}
#'  \item{\code{Ni }}{Concentration of nickel (in mg/kg)}
#'  \item{\code{P }}{Concentration of phosphorus (in mg/kg)}
#'  \item{\code{Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{Pb204 }}{Concentration of lead, 204 neutrons (in mg/kg)}
#'  \item{\code{Pb206 }}{Concentration of lead, 206 neutrons (in mg/kg)}
#'  \item{\code{Pb207 }}{Concentration of lead, 207 neutrons (in mg/kg)}
#'  \item{\code{Pb208 }}{Concentration of lead, 208 neutrons (in mg/kg)}
#'  \item{\code{X6_7Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X7_8Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X6_4Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X7_4Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X8_4Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{Pd }}{Concentration of palladium (in mg/kg)}
#'  \item{\code{Pt }}{Concentration of platium (in mg/kg)}
#'  \item{\code{Rb }}{Concentration of rubidium (in mg/kg)}
#'  \item{\code{Re }}{Concentration of rhenium (in mg/kg)}
#'  \item{\code{S }}{Concentration of sulfur (in mg/kg)}
#'  \item{\code{Sb }}{Concentration of antimony (in mg/kg)}
#'  \item{\code{Sc }}{Concentration of scandium (in mg/kg)}
#'  \item{\code{Se }}{Concentration of selenium (in mg/kg)}
#'  \item{\code{Sn }}{Concentration of tin (in mg/kg)}
#'  \item{\code{Sr }}{Concentration of strontium (in mg/kg)}
#'  \item{\code{Ta }}{Concentration of tantalum (in mg/kg)}
#'  \item{\code{Te }}{Concentration of tellurium (in mg/kg)}
#'  \item{\code{Th }}{Concentration of thorium (in mg/kg)}
#'  \item{\code{Ti }}{Concentration of titanium (in mg/kg)}
#'  \item{\code{Tl }}{Concentration of thalium (in mg/kg)}
#'  \item{\code{U }}{Concentration of uranium (in mg/kg)}
#'  \item{\code{V }}{Concentration of vanadium (in mg/kg)}
#'  \item{\code{W }}{Concentration of tungsten (in mg/kg)}
#'  \item{\code{Y }}{Concentration of yttrium (in mg/kg)}
#'  \item{\code{Zn }}{Concentration of zinc (in mg/kg)}
#'  \item{\code{Zr }}{Concentration of zirconium (in mg/kg)}
#' }
#'
#' @name trondelagC
#' @docType data
#' @usage data(trondelagC)
#' @author NGU, \url{http://www.ngu.no}, transfered to R by Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A data frame with 754 observations and 70 variables
#' @references C.Reimann, J.Schilling, D.Roberts, K.Fabian. A regional-scale geochemical survey of soil C horizon samples in Nord-Trondelag, Central Norway. Geology and mineral potential, \emph{Applied Geochemistry} 61 (2015) 192-205.
#' @details The samples were analysed using aqua regia extraction. 
#' Sampling was based on a 6.6km grid, i.e. 1 sample site/36 km2. 
#' @keywords data
#' @examples 
#' 
#' data(trondelagC)
#' str(trondelagC)
NULL


#' regional geochemical survey of soil O in Norway
#' 
#' A regional-scale geochemical survey of O horizon samples in Nord-Trondelag, Central Norway
#'
#' \itemize{
#'  \item{\code{X.Loc_ID }}{ID}
#'  \item{\code{LITHO }}{Rock type}
#'  \item{\code{longitude }}{langitude in WGS84}
#'  \item{\code{latitude }}{latitude in WGS84}
#'  \item{\code{E32wgs }}{UTM zone east}
#'  \item{\code{N32wgs }}{UTM zone north}
#'  \item{\code{X.Medium }}{a numeric vector}
#'  \item{\code{Alt_masl }}{a numeric vector}
#'  \item{\code{LOI_480 }}{Loss on ignition}
#'  \item{\code{pH }}{Numeric scale used to specify the acidity or alkalinity of an aqueous solution}
#'  \item{\code{Ag }}{Concentration of silver (in mg/kg)}
#'  \item{\code{Al }}{Concentration of aluminum (in mg/kg)}
#'  \item{\code{As }}{Concentration of arsenic (in mg/kg)}
#'  \item{\code{Au }}{Concentration of gold (in mg/kg)}
#'  \item{\code{B }}{Concentration of boron (in mg/kg)}
#'  \item{\code{Ba }}{Concentration of barium (in mg/kg)}
#'  \item{\code{Be }}{Concentration of beryllium (in mg/kg)}
#'  \item{\code{Bi }}{Concentration of bismuth (in mg/kg)}
#'  \item{\code{Ca }}{Concentration of calzium (in mg/kg)}
#'  \item{\code{Cd }}{Concentration of cadmium (in mg/kg)}
#'  \item{\code{Ce }}{Concentration of cerium (in mg/kg)}
#'  \item{\code{Co }}{Concentration of cobalt (in mg/kg)}
#'  \item{\code{Cr }}{Concentration of chromium (in mg/kg)}
#'  \item{\code{Cs }}{Concentration of cesium (in mg/kg)}
#'  \item{\code{Cu }}{Concentration of copper (in mg/kg)}
#'  \item{\code{Fe }}{Concentration of iron (in mg/kg)}
#'  \item{\code{Ga }}{Concentration of gallium (in mg/kg)}
#'  \item{\code{Ge }}{Concentration of germanium (in mg/kg)}
#'  \item{\code{Hf }}{Concentration of hafnium (in mg/kg)}
#'  \item{\code{Hg }}{Concentration of mercury (in mg/kg)}
#'  \item{\code{In }}{Concentration of indium (in mg/kg)}
#'  \item{\code{K }}{Concentration of pottasium (in mg/kg)}
#'  \item{\code{La }}{Concentration of lanthanum (in mg/kg)}
#'  \item{\code{Li }}{Concentration of lithium (in mg/kg)}
#'  \item{\code{Mg }}{Concentration of magnesium (in mg/kg)}
#'  \item{\code{Mn }}{Concentration of manganese (in mg/kg)}
#'  \item{\code{Mo }}{Concentration of molybdenum (in mg/kg)}
#'  \item{\code{Na }}{Concentration of sodium (in mg/kg)}
#'  \item{\code{Nb }}{Concentration of niobium (in mg/kg)}
#'  \item{\code{Ni }}{Concentration of nickel (in mg/kg)}
#'  \item{\code{P }}{Concentration of phosphorus (in mg/kg)}
#'  \item{\code{Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{Pb204 }}{Concentration of lead, 204 neutrons (in mg/kg)}
#'  \item{\code{Pb206 }}{Concentration of lead, 206 neutrons (in mg/kg)}
#'  \item{\code{Pb207 }}{Concentration of lead, 207 neutrons (in mg/kg)}
#'  \item{\code{Pb208 }}{Concentration of lead, 208 neutrons (in mg/kg)}
#'  \item{\code{X6_7Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X7_8Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X6_4Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X7_4Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{X8_4Pb }}{Concentration of lead (in mg/kg)}
#'  \item{\code{Pd }}{Concentration of palladium (in mg/kg)}
#'  \item{\code{Pt }}{Concentration of platium (in mg/kg)}
#'  \item{\code{Rb }}{Concentration of rubidium (in mg/kg)}
#'  \item{\code{Re }}{Concentration of rhenium (in mg/kg)}
#'  \item{\code{S }}{Concentration of sulfur (in mg/kg)}
#'  \item{\code{Sb }}{Concentration of antimony (in mg/kg)}
#'  \item{\code{Sc }}{Concentration of scandium (in mg/kg)}
#'  \item{\code{Se }}{Concentration of selenium (in mg/kg)}
#'  \item{\code{Sn }}{Concentration of tin (in mg/kg)}
#'  \item{\code{Sr }}{Concentration of strontium (in mg/kg)}
#'  \item{\code{Ta }}{Concentration of tantalum (in mg/kg)}
#'  \item{\code{Te }}{Concentration of tellurium (in mg/kg)}
#'  \item{\code{Th }}{Concentration of thorium (in mg/kg)}
#'  \item{\code{Ti }}{Concentration of titanium (in mg/kg)}
#'  \item{\code{Tl }}{Concentration of thalium (in mg/kg)}
#'  \item{\code{U }}{Concentration of uranium (in mg/kg)}
#'  \item{\code{V }}{Concentration of vanadium (in mg/kg)}
#'  \item{\code{W }}{Concentration of tungsten (in mg/kg)}
#'  \item{\code{Y }}{Concentration of yttrium (in mg/kg)}
#'  \item{\code{Zn }}{Concentration of zinc (in mg/kg)}
#'  \item{\code{Zr }}{Concentration of zirconium (in mg/kg)}
#' }
#'
#' @name trondelagO
#' @docType data
#' @usage data(trondelagO)
#' @author NGU, \url{http://www.ngu.no}, transfered to R by Matthias Templ \email{matthias.templ@@tuwien.ac.at}
#' @format A data frame with 754 observations and 70 variables
#' @references C.Reimann, J.Schilling, D.Roberts, K.Fabian. A regional-scale geochemical survey of soil C horizon samples in Nord-Trondelag, Central Norway. Geology and mineral potential, \emph{Applied Geochemistry} 61 (2015) 192-205.
#' @details The samples were analysed using aqua regia extraction. 
#' Sampling was based on a 6.6km grid, i.e. 1 sample site/36 km2. 
#' @keywords data
#' @examples 
#' 
#' data(trondelagO)
#' str(trondelagO)
NULL


#' unemployed of young people
#' 
#' Youth not in employment, education or training (NEET) in 43 countries from 1997 till 2015
#' 
#' This indicator presents the share of young people who are 
#' not in employment, education or training (NEET), as 
#' a percentage of the total number of young people 
#' in the corresponding age group, by gender. 
#' Young people in education include those attending 
#' part-time or full-time education, but exclude those 
#' in non-formal education and in educational activities 
#' of very short duration. Employment is defined according 
#' to the OECD/ILO Guidelines and covers all those who 
#' have been in paid work for at least one hour in the 
#' reference week of the survey or were temporarily 
#' absent from such work. Therefore NEET youth can be 
#' either unemployed or inactive and not involved in 
#' education or training. Young people who are neither 
#' in employment nor in education or training are at 
#' risk of becoming socially excluded - individuals 
#' with income below the poverty-line and lacking the 
#' skills to improve their economic situation.
#' 
#' @name unemployed
#' @docType data
#' @format A (tidy) data frame with 1216 observations on the following 4 variables.
#' \itemize{ 
#' \item{\code{country }}{Country of origin}
#' \item{\code{age }}{age group } 
#' \item{\code{year }}{Year} 
#' \item{\code{value }}{percentage of unemployed}
#' }
#' @author translated from \url{https://data.oecd.org/} and restructured by Matthias Templ
#' @references OECD (2017), Youth not in employment, education or training (NEET) (indicator). doi: 10.1787/72d1033a-en (Accessed on 27 March 2017)
#' @source OECD:
#' \url{https://data.oecd.org/}
#' @keywords datasets
#' @examples
#' 
#' data(unemployed)
#' str(unemployed)
NULL




