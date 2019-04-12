#' CoDaCuCoord
#' 
#' @title Coordinate representation of a compositional cube
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @author Kamila Facevicova
#' @references Facevicova, K., Filzmoser, P. and K. Hron (2019) Compositional Cubes: Three-factorial Compositional Data. Under review.
#' @description CoDaCuCoord computes a system of orthonormal coordinates of a compositional cube. 
#' Computation of either pivot coordinates or a coordinate system based on the given SBP is possible.
#' 
#' @param x a data frame containing variables representing row, column and slice factors of the respective compositional cube and variable with the values of the composition.
#' @param row.factor name of the variable representing the row factor. Needs to be stated with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be stated with the quotation marks.
#' @param slice.factor name of the variable representing the slice factor. Needs to be stated with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be stated with the quotation marks.
#' @param SBPr an \eqn{I-1\times I} array defining the sequential binary partition of the values of the row factor, where I is the number of the row factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPc an \eqn{J-1\times J} array defining the sequential binary partition of the values of the column factor, where J is the number of the column factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPs an \eqn{K-1\times K} array defining the sequential binary partition of the values of the slice factor, where K is the number of the slice factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param pivot logical, default is FALSE. If TRUE, or one of the SBPs is not defined, its pivot version is used.
#' @param print.res logical, default is FALSE. If TRUE, the output is displayed in the Console.
#' @details This transformation moves the IJK-part compositional cubes from the simplex into a (IJK-1)-dimensional real space isometrically with respect to its three-factorial nature.
#' @keywords multivariate, coordinates
#' @export
#' @seealso 
#' \code{\link{CoDaTaCoord}} 
#' \code{\link{CoDaTaCoordWrapper}} 
#' \code{\link{CoDaCuCoordWrapper}}
#' @return 
#' \item{Coordinates}{an array of orthonormal coordinates.} 
#' \item{Grap.rep}{graphical representation of the coordinates. 
#' Parts denoted by + form the groups in the numerator of the respective computational formula, 
#' parts - form the denominator and parts . are not involved in the given coordinate.} 
#' \item{Row.balances}{an array of row balances.}
#' \item{Column.balances}{an array of column balances.}
#' \item{Slice.balances}{an array of slice balances.}
#' \item{Row.column.OR}{an array of row-column OR coordinates.}
#' \item{Row.slice.OR}{an array of row-slice OR coordinates.}
#' \item{Column.slice.OR}{an array of column-slice OR coordinates.}
#' \item{Row.col.slice.OR}{an array of coordinates describing the mutual interaction between all three factors.}
#' \item{Contrast.matrix}{contrast matrix.}
#' \item{Log.ratios}{an array of pure log-ratios between groups of parts without the normalizing constant.}
#' \item{Coda.cube}{cube form of the given composition.}
#' @examples 
#' ### example from Fa\v cevicov\'a (2019)
#' data(employment2)
#' CZE <- employment2[which(employment2$Country == 'CZE'), ]
#' 
#' # pivot coordinates
#' CoDaCuCoord(CZE, "Sex", 'Contract', "Age", 'Value')
#' 
#' # coordinates with given SBP
#' 
#' r <- t(c(1,-1))
#' c <- t(c(1,-1))
#' s <- rbind(c(1,-1,-1), c(0,1,-1))
#' 
#' CoDaCuCoord(CZE, "Sex", 'Contract', "Age", 'Value', r,c,s)
CoDaCuCoord <- function(x, row.factor=NULL, col.factor=NULL, slice.factor=NULL, value=NULL, SBPr=NULL, SBPc=NULL, SBPs=NULL, pivot=FALSE, print.res=FALSE)
{                                                                             
  
  # Control and subsidiary parameters setting
  
  if(is.null(row.factor)) stop('Name of the row factor is not defined!')
  if(is.null(col.factor)) stop('Name of the column factor is not defined!')
  if(is.null(slice.factor)) stop('Name of the slice factor is not defined!')
  if(is.null(value)) stop('Name of the value variable is not defined!')
  
  x[,row.factor] <- as.factor(x[,row.factor])
  x[,col.factor] <- as.factor(x[,col.factor])
  x[,slice.factor] <- as.factor(x[,slice.factor])
  I <- nlevels(x[,row.factor]) # number of row factor levels
  J <- nlevels(x[,col.factor]) # number of column factor levels
  K <- nlevels(x[,slice.factor]) # number of slice factor levels
  y <- x[,c(row.factor, col.factor, slice.factor, value)]
  
  x_vec <- y[order(y[,1], y[,2], y[,3]),4] # vectorized cube according to Facevicova19
  
  if(!identical(as.numeric(table(x[,c(row.factor, col.factor)])),as.numeric(rep(K,I*J)))) stop('The CoDa Cube x is not defined properly, some values are missing!')
  if(!is.null(SBPr)&(nrow(SBPr)!= (I-1)||ncol(SBPr)!=I)) 
  {warning('The row SBP is not defined properly, pivot coordinates are used!')
    SBPr <- NULL}
  if(!is.null(SBPc)&(nrow(SBPc)!= (J-1)||ncol(SBPc)!=J)) 
  {warning('The column SBP is not defined properly, pivot coordinates are used!')
    SBPc <- NULL}
  if(!is.null(SBPs)&(nrow(SBPs)!= (K-1)||ncol(SBPs)!=K)) 
  {warning('The slice SBP is not defined properly, pivot coordinates are used!')
    SBPs <- NULL}
  
  ### Definition of pivot SBP (if necessary) 
  
  if(is.null(SBPr)||pivot==TRUE)
  {
    SBPr <- numeric()
    for(j in 1:(I-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, I-j))
      SBPr <- rbind(SBPr, novy)
    }
    #print("SBP of row factor is not defined, its pivot version is used!")
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
    #print("SBP of column factor is not defined, its pivot version is used!")
  }
  rownames(SBPc) <- NULL
  
  if(is.null(SBPs)||pivot==TRUE)
  {
    SBPs <- numeric()
    for(j in 1:(K-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, K-j))
      SBPs <- rbind(SBPs, novy)
    }
    #print("SBP of slice factor is not defined, its pivot version is used!")
  }
  rownames(SBPs) <- NULL
  
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
  
  norm_const_balance  <- function(x){ #based on vector of contrasts
    r <- length(which(x>0))
    s <- length(which(x<0))
    koef <- sqrt((r*s)/(r+s))
    return(koef)
  }
  
  norm_const_OR <- function(x){ #based on vector of contrasts
    kladne = table(x[which(x>0)])
    celkem = length(which(x!=0))
    if(dim(kladne)==2)
      koef=sqrt((kladne[1]*kladne[2])/celkem)
    else
      koef=sqrt((kladne*kladne/4)/celkem)
    names(koef) = NULL
    return(koef)
  }
  
  norm_const_ORR <- function(x,y,z){ #based on SBPr, SBPc, SBPs
    A = length(which(x>0))*length(which(y>0))*length(which(z>0))
    H = length(which(x<0))*length(which(y<0))*length(which(z<0))
    vse = length(which(x!=0))*length(which(y!=0))*length(which(z!=0))
    koef=sqrt(A*H/vse)
    return(koef)
  }
  
  ### expansion of SBP to the whole table
  
  SBPr_cele <- matrix(SBPr[,rep(c(1:I), each=J*K)], ncol=I*J*K)
  SBPc_cele <- matrix(SBPc[,rep(rep(c(1:J), each=K), I)], ncol=I*J*K)
  SBPs_cele <- matrix(SBPs[,rep(c(1:K),I*J)], ncol=I*J*K)
  
  ### Generating vectors of:
  
  # balances
  LCr <- t(apply(SBPr_cele, 1, FUN=log_kontrasty))
  LCc <- t(apply(SBPc_cele, 1, FUN=log_kontrasty))
  LCs <- t(apply(SBPs_cele, 1, FUN=log_kontrasty))
  
  # pairwise interaction coordinates
  OR_deleni_r_c <- NULL
  for(i in 1:(I-1))
  {
    for(j in 1:(J-1))
    {
      novy <- LCr[i,]*LCc[j,] 
      OR_deleni_r_c <- rbind(OR_deleni_r_c, novy)
    }
  }
  
  rownames(OR_deleni_r_c) <- NULL
  
  OR_deleni_r_s <- NULL
  for(i in 1:(I-1))
    
  {
    for(k in 1:(K-1))
    {
      novy <- LCr[i,]*LCs[k,] 
      OR_deleni_r_s <- rbind(OR_deleni_r_s, novy)
    }
  }
  
  rownames(OR_deleni_r_s) <- NULL
  
  OR_deleni_s_c <- NULL
  for(k in 1:(K-1))
  {
    for(j in 1:(J-1))
    {
      novy <- LCs[k,]*LCc[j,] 
      OR_deleni_s_c <- rbind(OR_deleni_s_c, novy)
    }
  }
  
  rownames(OR_deleni_s_c) <- NULL
  
  # full interaction coordinates
  
  OR_deleni_r_c_s <- NULL
  norm.constants.ORR <- NULL
  for(i in 1:(I-1))
  {
    for(j in 1:(J-1))
    {
      for(k in 1:(K-1))
      {
        novy <- LCr[i,]*LCc[j,]*LCs[k,] 
        OR_deleni_r_c_s <- rbind(OR_deleni_r_c_s, novy)
        
        const.nova <- norm_const_ORR(SBPr[i,],SBPc[j,],SBPs[k,])
        norm.constants.ORR <- c(norm.constants.ORR, const.nova)
      }
    }
  }
  
  rownames(OR_deleni_r_c_s) <- NULL
  
  OR_deleni <- rbind(OR_deleni_r_c, OR_deleni_r_s, OR_deleni_s_c, OR_deleni_r_c_s)
  
  ### matrix with generating vectors. Important for the back-transformation!
  
  normovani <- function(x){x/(norm(as.matrix(x), type="f"))}
  OR_contrasts <- t(apply(OR_deleni, 1, FUN=normovani))
  contrasts <- rbind(LCr, LCc, LCs, OR_contrasts)
  
  coord.names <- c(paste('z', 1:(I-1), '^r', sep=''), paste('z', 1:(J-1), '^c', sep=''), paste('z', 1:(K-1), '^s', sep=''),
                   paste('z', sort(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rc', sep=''),
                   paste('z', sort(outer(c(1:(I-1)),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rs', sep=''),
                   paste('z', sort(outer(c(1:(J-1)),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^cs', sep=''),
                   paste('z', sort(outer(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep='')),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rcs', sep=''))
  
  rownames(contrasts) <- coord.names
  colnames(contrasts) <- c(paste('x', sort(outer(outer(c(1:(I)),c(1:(J)), FUN=function(x,y)paste(x,y,sep='')),c(1:(K)), FUN=function(x,y)paste(x,y,sep=''))), sep=''))
  
  ### Coordinates
  
  souradnice <- contrasts%*%log(x_vec)
  rownames(souradnice) <- coord.names
  
  ### Pure log-ratios between groups of parts (without normalizing constant)
  norm.constants.balance = apply(contrasts[1:(I+J+K-3), ], 1, norm_const_balance)
  norm.constants.OR = apply(contrasts[(I+J+K-2):(I*J+I*K+K*J-I-J-K), ], 1, norm_const_OR)
  # norm. constants for ORR coordinates were already computed with these coordinates
  norm.constants = c(norm.constants.balance, norm.constants.OR, norm.constants.ORR)
  
  log.ratios = souradnice/norm.constants
  
  ### Table form of the CoDa table
  tab0 <- unite(y, slice.factor, col.factor, col='col_slice')
  tab <-  spread(tab0, 'col_slice', value)[,-1]
  colnames(tab) <- levels(as.factor(tab0[,'col_slice']))
  rownames(tab) <- levels(tab0[,row.factor])
  
  ### Graphical representation of groups within table:
  grap.rep <- list()
  permutation <- order(tab0[,1])
  for(i in 1:nrow(contrasts))
  {
    grap.r <- rep(".",ncol(contrasts))
    grap.r[which(contrasts[i,]>0)] <- "+"
    grap.r[which(contrasts[i,]<0)] <- "-"
    grap.r <- data.frame(tab0[permutation,c(1,2)], grap.r)
    grap.r.tab  <- spread(grap.r, 'col_slice', grap.r)[,-1]
    row.names(grap.r.tab) <- levels(tab0[,row.factor])
    grap.rep[[i]] <- grap.r.tab
  }
  
  names(grap.rep) <- coord.names
  
  ### Result:
  
  if(print.res==TRUE)
  {
    print("Row balances:")
    print(souradnice[c(1:(I-1))])
    print(grap.rep[c(1:(I-1))])
    
    print("Column balances:")
    print(souradnice[c(I:(I+J-2))])
    print(grap.rep[c(I:(I+J-2))])
    
    print("Slice balances:")
    print(souradnice[c((I+J-1):(I+J+K-3))])
    print(grap.rep[c((I+J-1):(I+J+K-3))])
    
    print("Row and Column odds ratio coordinates:")
    print(souradnice[c((I+J+K-2):(I*J+K-2))])
    print(grap.rep[c((I+J+K-2):(I*J+K-2))])
    
    print("Row and Slice odds ratio coordinates:")
    print(souradnice[c((I*J+K-1):(I*J+I*K-I-1))])
    print(grap.rep[c((I*J+K-1):(I*J+I*K-I-1))])
    
    print("Column and Slice odds ratio coordinates:")
    print(souradnice[c((I*J+I*K-I):(I*J+I*K+K*J-I-J-K))])
    print(grap.rep[c((I*J+I*K-I):(I*J+I*K+K*J-I-J-K))])
    
    print("Row, Column and Slice odds ratio coordinates:")
    print(souradnice[c((I*J+I*K+K*J-I-J-K+1):(I*J*K-1))])
    print(grap.rep[c((I*J+I*K+K*J-I-J-K+1):(I*J*K-1))])
  }
  
  result <- list("Coordinates"=souradnice, "Grap.rep" = grap.rep, "Row.balances"=souradnice[c(1:(I-1)),1],
                 "Column.balances"=souradnice[c(I:(I+J-2)),1], "Slice.balances"=souradnice[c((I+J-1):(I+J+K-3)),1],
                 "Row.column.OR"=souradnice[c((I+J+K-2):(I*J+K-2)),1], "Row.slice.OR"=souradnice[c((I*J+K-1):(I*J+I*K-I-1)),1],
                 "Column.slice.OR"=souradnice[c((I*J+I*K-I):(I*J+I*K+K*J-I-J-K)),1], "Row.col.slice.OR"=souradnice[c((I*J+I*K+K*J-I-J-K+1):(I*J*K-1)),1],
                 'Log.ratios'=log.ratios, "Contrast.matrix" = contrasts, 'Coda.cube'=tab)
  return(result)
  
}


#####################
### Examples 

# load('employment.rda')
# CZE <- employment[which(employment$Country=='CZE'),]

### pivot coordinates
# CoDaCuCoord(CZE, "Sex", 'Contract', "Age", 'Value')

### coordinates with given SBP

# r <- t(c(1,-1))
# c <- t(c(1,-1))
# s <- rbind(c(1,-1,-1), c(0,1,-1))

# CoDaCuCoord(CZE, "Sex", 'Contract', "Age", 'Value', r,c,s)
