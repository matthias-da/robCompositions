#' alcoholreg 
#' 
#' Regional alcohol per capita (15+) consumption by WHO region
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



#' alcohol 
#' 
#' Alcohol consumptions by country and type of alcohol
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


#' Production 
#' 
#' Production splitted by nationality on enterprise level
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

#' Special payments
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
#' ternaryDiag(arcticLake)
#' plot(isomLR(arcticLake))
NULL

#' Child, middle and eldery population
#' 
#' Percentages of childs, middle generation and eldery population in 195 countries.
#'
#' \itemize{
#'  \item{\code{<15 }}{numeric vector of percentages of sand}
#'  \item{\code{15-60 }}{numeric vector of percentages of silt}
#'  \item{\code{60+ }}{numeric vector of percentages of clay}
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
#' plot(isomLR(ageCatWorld[, 1:3]))
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

#' Economic indicators
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

#' Election data
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



#' Gemas geochemical data set
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



#' Regional geochemical survey of soil C in Norway
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


#' Regional geochemical survey of soil O in Norway
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


#' Metabolomics mcad data set
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


#' Mortality and life expectancy in the EU
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
#' @source Eurostat
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
#' phdImputed <- impCoda(phd[, 7:ncol(phd)])$xOrig
#' 
NULL


#' Labour force by status in employment 
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


#' Hospital discharges on cancer and distribution of age
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


#' Life expectancy and GDP (2008) for EU-countries
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
