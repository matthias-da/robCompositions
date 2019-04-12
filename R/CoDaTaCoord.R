#' CoDaTaCoord
#' 
#' @title Coordinate representation of a compositional cube
#' @author Kamila Facevicova
#' @references Facevicova, K., Hron, K., Todorov, V. and M. Templ (2018) 
#' General approach to coordinate representation of compositional tables. 
#' Scandinavian Journal of Statistics, 45(4), 879--899.
#' @description CoDaTaCoord computes a system of orthonormal coordinates of a compositional table. 
#' Computation of either pivot coordinates or a coordinate system based on the given SBP is possible.
#' 
#' @param x a data frame containing variables representing row and column factors of the respective compositional table and variable with the values of the composition.
#' @param row.factor name of the variable representing the row factor. Needs to be stated with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be stated with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be stated with the quotation marks.
#' @param SBPr an \eqn{I-1\times I} array defining the sequential binary partition of the values of the row factor, where I is the number of the row factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPc an \eqn{J-1\times J} array defining the sequential binary partition of the values of the column factor, where J is the number of the column factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param pivot logical, default is FALSE. If TRUE, or one of the SBPs is not defined, its pivot version is used.
#' @param print.res logical, default is FALSE. If TRUE, the output is displayed in the Console.
#' @details This transformation moves the IJ-part compositional tables 
#' from the simplex into a (IJ-1)-dimensional real space isometrically 
#' with respect to its two-factorial nature. 
#' The coordinate system is formed by two types of coordinates - balances and log odds-ratios.
#' @keywords multivariate, coordinates
#' @export
#' @seealso 
#' \code{\link{CoDaCuCoord}} 
#' \code{\link{CoDaTaCoordWrapper}} 
#' \code{\link{CoDaCuCoordWrapper}}
#' @return 
#' \item{Coordinates}{an array of orthonormal coordinates.}
#' \item{Grap.rep}{graphical representation of the coordinates. Parts denoted by \verb{+} form the groups in the numerator of the respective computational formula, parts \verb{-} form the denominator and parts \verb{.} are not involved in the given coordinate.}
#' \item{Ind.coord}{an array of row and column balances. Coordinate representation of the independent part of the table.}
#' \item{Int.coord}{an array of OR coordinates. Coordinate representation of the interactive part of the table.}
#' \item{Contrast.matrix}{contrast matrix.}
#' \item{Log.ratios}{an array of pure log-ratios between groups of parts without the normalizing constant.}
#' \item{Coda.table}{table form of the given composition.}
#' @examples 
#' # example from Fa\v cevicov\'a (2018):
#' data(manu_abs)
#' manu_USA <- manu_abs[which(manu_abs$country=='USA'),]
#' manu_USA$output <- factor(manu_USA$output, levels=c('LAB', 'SUR', 'INP'))
#' 
#' # pivot coordinates
#' CoDaTaCoord(manu_USA, row.factor = 'output', col.factor = 'isic', value='value')
#' 
#' # SBPs defined in paper
#' r <- rbind(c(-1,-1,1), c(-1,1,0))
#' c <- rbind(c(-1,-1,-1,-1,1), c(-1,-1,-1,1,0), c(-1,-1,1,0,0), c(-1,1,0,0,0))
#' CoDaTaCoord(manu_USA, row.factor = 'output', col.factor = 'isic', value='value', SBPr=r, SBPc=c)
CoDaTaCoord <- function(x=NULL, row.factor=NULL, col.factor=NULL, 
                        value=NULL, SBPr=NULL, SBPc=NULL, 
                        pivot=FALSE, print.res=FALSE){
  
  # Control and subsidiary parameters setting
  
  if(is.null(row.factor)) stop('Name of the row factor is not defined!')
  if(is.null(col.factor)) stop('Name of the column factor is not defined!')
  if(is.null(value)) stop('Name of the value variable is not defined!')
  
  x[,row.factor] <- as.factor(x[,row.factor])
  x[,col.factor] <- as.factor(x[,col.factor])
  I <- nlevels(x[,row.factor]) # number of row factor levels
  J <- nlevels(x[,col.factor]) # number of column factor levels
  y <- x[,c(row.factor, col.factor, value)]
  x_vec <- y[order(y[,1], y[,2]),3] # vectorized table
  
  if(!identical(as.numeric(table(x[,row.factor])),as.numeric(rep(J,I)))) stop('The CoDa Table x is not defined properly, some values are missing!')
  if(!is.null(SBPr)&(nrow(SBPr)!= (I-1)||ncol(SBPr)!=I)) 
  {warning('The row SBP is not defined properly, pivot coordinates are used!')
    SBPr <- NULL}
  if(!is.null(SBPc)&(nrow(SBPc)!= (J-1)||ncol(SBPc)!=J)) 
  {warning('The column SBP is not defined properly, pivot coordinates are used!')
    SBPc <- NULL}
  
  ### Definition of pivot SBP (if necessary)
  
  if(is.null(SBPr)||pivot==TRUE)
  {
    SBPr <- numeric()
    for(j in 1:(I-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, I-j))
      SBPr <- rbind(SBPr, novy)
    }
  }
  rownames(SBPr) <- NULL
  
  
  if(is.null(SBPc)||pivot==TRUE)
  {
    SBPc <- numeric()
    for(j in 1:(J-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, J-j))
      SBPc <- rbind(SBPc, novy)
    }
  }
  rownames(SBPc) <- NULL
  
  
  log_kontrasty <- function(x){
    r <- length(which(x==1))
    s <- length(which(x==-1))
    koef1 <- sqrt((r*s)/(r+s))*(1/r)
    koef2 <- -sqrt((r*s)/(r+s))*(1/s)
    log_kontrast <- rep(0, length(x))
    log_kontrast[which(x==1)] <- koef1
    log_kontrast[which(x==-1)] <- koef2
    return(log_kontrast)
  }
  
  norm_const_balance  <- function(x){
    r <- length(which(x>0))
    s <- length(which(x<0))
    koef <- sqrt((r*s)/(r+s))
    return(koef)
  }
  
  norm_const_OR <- function(x){
    kladne = table(x[which(x>0)])
    celkem = length(which(x!=0))
    if(dim(kladne)==2)
      koef=sqrt((kladne[1]*kladne[2])/celkem)
    else
      koef=sqrt((kladne*kladne/4)/celkem)
    names(koef) = NULL
    return(koef)
  }
  
  
  ### expansion of SBP to the whole table
  SBPr_cele <- matrix(SBPr[,rep(c(1:I), rep(J,I))], ncol=I*J) 
  SBPc_cele <- matrix(SBPc[,rep(c(1:J), I)], ncol=I*J)
  
  ### generating vectors of 
  
  # balances
  LCr <- t(apply(SBPr_cele, 1, FUN=log_kontrasty))
  LCc <- t(apply(SBPc_cele, 1, FUN=log_kontrasty))
  
  # OR coordinates
  OR_deleni <- NULL
  for(i in 1:(I-1))
  {
    for(j in 1:(J-1))
    {
      novy <- LCr[i,]*LCc[j,] 
      OR_deleni <- rbind(OR_deleni, novy)
    }
  }
  
  rownames(OR_deleni) <- NULL
  
  OR_contrasts <- t(apply(OR_deleni, 1, FUN=function(x){x/(norm(as.matrix(x), type="f"))}))
  
  ### matrix with generating vectors. Important for the back-transformation!
  contrasts <- rbind(LCr, LCc, OR_contrasts)
  
  ### Coordinates:
  coord.names <- c(paste('z', 1:(I-1), '^r', sep=''), paste('z', 1:(J-1), '^c', sep=''), paste('z', sort(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep=''))), '^OR', sep=''))
  
  souradnice <- contrasts%*%log(x_vec)
  rownames(souradnice) <- coord.names
  
  ind.coord <- souradnice[c(1:(I+J-2))]
  int.coord <- souradnice[c((I+J-1):(I*J-1))]
  
  names(ind.coord) <- coord.names[c(1:(I+J-2))]
  names(int.coord) <- coord.names[c((I+J-1):(I*J-1))]
  
  rownames(contrasts) <- coord.names
  colnames(contrasts) <- c(paste('x', sort(outer(c(1:I),c(1:J), FUN=function(x,y)paste(x,y,sep=''))), sep=''))
  
  ### Pure log-ratios between groups of parts (without normalizing constant)
  norm.constants.balance = apply(contrasts[1:(I+J-2), ], 1, norm_const_balance)
  norm.constants.OR = apply(contrasts[(I+J-1):(I*J-1), ], 1, norm_const_OR)
  norm.constants = c(norm.constants.balance, norm.constants.OR)
  log.ratios = souradnice/norm.constants
  
  ### Table form of the CoDa table
  tab <- spread(y, col.factor, value)[,-1]
  colnames(tab) <- levels(x[,col.factor])
  rownames(tab) <- levels(x[,row.factor])
  
  ### Graphical representation of groups within table:
  grap.rep <- array(NA, dim = c(I, J, I*J-1))
  dimnames(grap.rep)[[1]] <- levels(x[,row.factor])
  dimnames(grap.rep)[[2]] <- levels(x[,col.factor])
  dimnames(grap.rep)[[3]] <- coord.names
  for(i in 1:nrow(contrasts))
  {
    grap.r <- rep(".",ncol(contrasts))
    grap.r[which(contrasts[i,]>0)] <- "+"
    grap.r[which(contrasts[i,]<0)] <- "-"
    grap.rep[,,i] <- t(matrix(grap.r, ncol=I))
  }
  grap.rep <- noquote(grap.rep)
  
  ### Result
  if(print.res==TRUE)
  {
    print("Row balances:")
    print(souradnice[c(1:(I-1))])
    print(grap.rep[c(1:(I-1))])
    
    print("Column balances:")
    print(souradnice[c(I:(I+J-2))])
    print(grap.rep[c(I:(I+J-2))])
    
    print("Odds ratio coordinates:")
    print(souradnice[c((I+J-1):(I*J-1))])
    print(grap.rep[c((I+J-1):(I*J-1))])
  }
  
  result <- list("Coordinates"=souradnice, "Grap.rep" = grap.rep, "Ind.coord"=ind.coord, "Int.coord"=int.coord, "Contrast.matrix"=contrasts, 'Log.ratios'=log.ratios, 'Coda.table'=tab)
  return(result)
  
}


#####################
### Examples 

# load("manu_abs.rda")
# manu_USA <- manu_abs[which(manu_abs$country=='USA'),]
# manu_USA$output <- factor(manu_USA$output, levels=c('LAB', 'SUR', 'INP'))

### pivot coordinates
# CoDaTaCoord(manu_USA, row.factor = 'output', col.factor = 'isic', value='value')

### SBPs defined in paper
# r <- rbind(c(-1,-1,1), c(-1,1,0))
# c <- rbind(c(-1,-1,-1,-1,1), c(-1,-1,-1,1,0), c(-1,-1,1,0,0), c(-1,1,0,0,0))
# CoDaTaCoord(manu_USA, row.factor = 'output', col.factor = 'isic', value='value', SBPr=r, SBPc=c)
